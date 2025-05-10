;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014-2025 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2024 宋文武 <iyzsong@envs.net>
;;; Copyright © 2014, 2015, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2017, 2018, 2021 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Albin Söderqvist <albin@fripost.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016-2021, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Steve Webber <webber.sl@gmail.com>
;;; Copyright © 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2017, 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 okapi <okapi@firemail.cc>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Madalin Ionel-Patrascu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2020 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
;;; Copyright © 2019-2023 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019, 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Alberto Eleuterio Flores Guerrero <barbanegra+guix@posteo.mx>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
;;; Copyright © 2020 Vitaliy Shatrov <D0dyBo0D0dyBo0@protonmail.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Lu hux <luhux@outlook.com>
;;; Copyright © 2020 Tomás Ortín Fernández <tomasortin@mailbox.org>
;;; Copyright © 2021 Olivier Rojon <o.rojon@posteo.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021, 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021, 2024 David Pflug <david@pflug.io>
;;; Copyright © 2021, 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021, 2022, 2024 Noisytoot <ron@noisytoot.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022, 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2022 Roman Riabenko <roman@riabenko.com>
;;; Copyright © 2022, 2023, 2025 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2022 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2022-2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Hendursaga <hendursaga@aol.com>
;;; Copyright © 2022 Parnikkapore <poomklao@yahoo.com>
;;; Copyright © 2022 Cairn <cairn@pm.me>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2023 Ivana Drazovic <iv.dra@hotmail.com>
;;; Copyright © 2023, 2024 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2024 Sébastien Lerique <sl@eauchat.org>
;;; Copyright © 2024 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2024 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023-2025 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2025 Nigko Yerden <nigko.yerden@gmail.com>
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

(define-module (gnu packages games)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emulators)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pcre)
  #:autoload (gnu packages pascal) (fpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages squirrel)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system trivial)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public hexahop
  (package
    (name "hexahop")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/hexahop/" version "/"
                           "hex-a-hop-" version ".tar.gz"))

       (sha256
        (base32 "1mm069wpwc8nrrzfn2f64vh550634xlfws64bfmhqhx86vcikgw0"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-sdl-prefix="
                                  #$(this-package-input "sdl-union")))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list (sdl-union (list sdl sdl-mixer sdl-ttf))))
    (home-page "https://sourceforge.net/projects/hexahop/")
    (synopsis "Puzzle game navigating paths over hexagons")
    (description
     "Hex-a-hop is a puzzle game in which a girl has to destroy green hexagons
by stepping on them.")
    (license license:gpl2+)))

(define-public abe
  (package
    (name "abe")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/abe/abe/abe-" version
                           "/abe-" version ".tar.gz"))
       (sha256
        (base32 "1xvpnq1y6y48fn3pvn2lk0h1ilmalv7nb7awpid1g4jcq1sfmi6z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-data-dir="
                            (assoc-ref %outputs "out")
                            "/share/abe"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "sh" "autogen.sh")))
         (add-before 'build 'set-SDL
           ;; Set correct environment for SDL.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "sdl") "/include/SDL:"
                      (or (getenv "CPATH") "")))
             #t))
         (add-after 'install 'finalize-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share")))
               ;; Installation script does not copy game data files.
               (let ((data (string-append share "/abe")))
                 (for-each (lambda (dir)
                             (let ((target (string-append data "/" dir)))
                               (mkdir-p target)
                               (copy-recursively dir target)))
                           '("images" "maps" "sounds")))
               ;; Create desktop file.
               (let ((apps (string-append share "/applications")))
                 (mkdir-p apps)
                 (make-desktop-entry-file
                  (string-append apps "/abe.desktop")
                  #:name "Abe's Amazing Adventure"
                  #:exec ,name
                  #:categories '("AdventureGame" "Game")
                  #:keywords
                  '("side-scrolling" "adventure" "pyramid" "singleplayer")
                  #:comment
                  '(("de" "Ein sich seitwärts bewegendes Abenteuerspiel")
                    (#f "Side-scrolling game")))))
             #t)))))
    (native-inputs
     (list autoconf automake))
    (inputs
     `(("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)
       ("sdl" ,(sdl-union (list sdl sdl-mixer)))))
    (home-page "https://abe.sourceforge.net")
    (synopsis "Scrolling, platform-jumping, ancient pyramid exploring game")
    (description
     "Abe's Amazing Adventure is a scrolling,
platform-jumping, key-collecting, ancient pyramid exploring game, vaguely in
the style of similar games for the Commodore+4.")
    (license license:gpl2+)))

(define-public adanaxisgpl
  (let* ((version "1.2.5")
         (commit (string-append "ADANAXIS_"
                                (string-join (string-split version #\.) "_")
                                "_RELEASE_X11")))
    (package
      (name "adanaxisgpl")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mushware/adanaxis")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vbg17lzbm0xl9yy9qymd1vgpz6f7fbr2hffl2ap0nm4zg0mnafm"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Necessary for building with gcc >=4.7.
             (substitute* "src/Mushcore/MushcoreSingleton.h"
               (("SingletonPtrSet\\(new SingletonType\\);")
                "MushcoreSingleton::SingletonPtrSet(new SingletonType);"))
             ;; Avoid an "invalid conversion from const char* to char*" error.
             (substitute* "src/Platform/X11/PlatformMiscUtils.cpp"
               (("char \\*end, \\*result;")
                (string-append "const char *end;\n"
                               "char *result;")))
             ;; autogen.pl will throw misleading errors if these don't exist.
             (for-each mkdir-p '("src/MushSecret" "data-adanaxis"))
             ;; Create these missing files at the right later moment.
             (substitute* "autogen.pl"
               (("automake")
                "automake --add-missing"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-inputs
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively
                (dirname (search-input-directory inputs "pixelsrc"))
                "data-adanaxis")
               (copy-recursively
                (dirname (search-input-file inputs "MushBase.rb"))
                "data-adanaxis/mushruby")))
           (replace 'bootstrap
             (lambda _
               (invoke "perl" "autogen.pl" "adanaxis"
                       "--type=gpl" "--dist=debian")))
           (add-after 'install 'install-data
             ;; XXX This was copied from the original (pre-Git) adanaxisgpl
             ;; package.  While the game appears to play fine without it,
             ;; I cannot prove that it's not missing *something*, so keep it.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((share (string-append (assoc-ref outputs "out")
                                           "/share/" ,name "-" ,version)))
                 (copy-recursively (search-input-directory inputs "mush")
                                   (string-append share "/mush"))))))))
      (native-inputs
       (list (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/mushware/adanaxis-data")
                     ;; XXX There is a tag matching COMMIT, but it does not
                     ;; contain the .mush files installed by 'install-data.
                     ;; Use this later commit as long as we install them.
                     (commit "6a5b5ad8ee82c10e67bc4c12b16404944fd5754d")))
               (file-name (git-file-name "adanaxis-data" version))
               (sha256
                (base32 "15am9ziq1i53sz0r7sjh2z329b52fkzj6fz7ms1nqqzdfmp11r3d")))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/mushware/adanaxis-mushruby")
                     (commit commit)))
               (file-name (git-file-name "adanaxis-mushruby" version))
               (sha256
                (base32 "0pzcvchysjj37420rpvarky580gi5d6pfv93kwa91rg6m5r1zwks")))
             autoconf
             automake
             perl))
      (inputs
       (list expat
             freeglut
             glu
             libjpeg-turbo
             libogg
             libtiff
             libvorbis
             libx11
             libxcrypt
             libxext
             pcre
             sdl
             sdl-mixer))
      (home-page "https://www.mushware.com")
      (synopsis "Action game in four spatial dimensions")
      (description
       "Adanaxis is a fast-moving first person shooter set in deep space, where
the fundamentals of space itself are changed.  By adding another dimension to
space this game provides an environment with movement in four directions and
six planes of rotation.  Initially the game explains the 4D control system via
a graphical sequence, before moving on to 30 levels of gameplay with numerous
enemy, ally, weapon and mission types.  Features include simulated 4D texturing,
mouse and joystick control, and original music.")
      (license license:gpl2))))

(define-public anarch
  (let ((commit "2d78d0c69a3aac14dbd8f8aca62d0cbd9d27c860")
        (revision "1"))
    (package
      (name "anarch")
      (version (git-version "1.1d" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~drummyfish/Anarch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lg9r6q1davn5yj181ccygmvaigvm8fr9q2s1bc77a1vkz68vzdk"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ;no tests
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure) ;no configure script
                          (replace 'build
                            (lambda _
                              (invoke "./make.sh" "sdl")))
                          (replace 'install
                            (lambda _
                              (let ((bin (string-append #$output "/bin")))
                                (install-file "anarch" bin)))))))
      (inputs (list alsa-lib libxcursor libxrandr sdl2))
      (home-page "https://drummyfish.gitlab.io/anarch/")
      (synopsis "Public domain 90s-style shooter game")
      (description "Anarch is a small, completely public domain, 90s-style
Doom clone shooter game.")
      (license license:cc0))))

(define-public antimicrox
  (package
    (name "antimicrox")
    (version "3.4.1")
    (home-page "https://github.com/AntiMicroX/antimicrox")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04yb5nppn751asbihr90sqk5imamc937886lc24cihhgp0sila8y"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;Tests are unmaintained
      #:configure-flags #~(list "-DCHECK_FOR_UPDATES=NO" "-DWITH_TESTS=NO"
                                #$(string-append "-DANTIMICROX_PKG_VERSION="
                                                 version))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-installation-target
                     (lambda _
                       (substitute* "CMakeLists.txt"
                         (("/usr(/lib/udev/rules.d)" _ lib)
                          (string-append #$output lib))))))))
    (native-inputs (list extra-cmake-modules gettext-minimal itstool qttools))
    (inputs (list libxtst libx11 qtbase sdl2))
    (synopsis "Control your system with a gamepad")
    (description
     "AntiMicroX is a graphical program used to map gamepad keys to keyboard, mouse,
scripts, and macros under both X.org and Wayland.  With it you can control
your system using a gamepad or play games that don't natively support
gamepads.  It can also be used for generating SDL2 configurations.

For unprivileged access to input events, this package provides udev rules for
use with @code{udev-service-type}.")
    (license license:gpl3+)))

(define-public armagetronad
  (package
    (name "armagetronad")
    (version "0.2.9.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/armagetronad/stable/"
                                  version "/armagetronad-" version ".tbz"))
              (sha256
               (base32
                "0a6rlp2lj5bp7pg1yf8brjyb3mw7nqp3amj19wvz3xni21bbc31k"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-games"      ;don't nest everything in ‘games/’
                   "--disable-uninstall") ;pointless (and broken) in Guix
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'omit-broken-symlinks
                 ;; v0.2.9.2.0 added relative_path() which I think broke
                 ;; install_link().  It now creates a broken armagetronad-master
                 ;; symlink that causes the 'validate-runpath phase to fail.
                 (lambda _
                   (substitute* "batch/sysinstall.in"
                     (("^install_link .*BINDIR.*") "")))))))
    (native-inputs (list pkg-config))
    (inputs (list libxml2
                  (sdl-union (list sdl sdl-image sdl-mixer))
                  freeglut
                  libpng
                  libjpeg-turbo))
    (home-page "https://www.armagetronad.org")
    (synopsis "Tron clone in 3D")
    (description
     "Armagetron Advanced is a multiplayer game in 3d that
attempts to emulate and expand on the lightcycle sequence from the movie Tron.
It's an old school arcade game slung into the 21st century.  Highlights
include a customizable playing arena, HUD, unique graphics, and AI bots.  For
the more advanced player there are new game modes and a wide variety of
physics settings to tweak as well.")
    (license license:gpl2+)))

(define-public astromenace
  (package
    (name "astromenace")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/viewizard/astromenace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0683a6bb4rvbz3jaqs6pc4msy6l3vr7fafxi4nmbvziv5kr7x9sv"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:configure-flags
      #~(list (string-append "-DDATADIR=" #$output "/share/astromenace"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            ;; Upstream provides no install phase.
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (share (string-append #$output "/share"))
                     (apps (string-append share "/applications"))
                     (data (string-append share "/astromenace"))
                     (icons (string-append share "/icons/hicolor/64x64/apps")))
                (install-file "astromenace" bin)
                (install-file "gamedata.vfs" data)
                (with-directory-excursion (string-append #$source "/share")
                  (install-file "astromenace.desktop" apps)
                  (mkdir-p icons)
                  (copy-file "astromenace_64.png"
                             (string-append icons "/astromenace.png")))))))))
    (inputs
     (list freealut
           freetype
           glu
           libogg
           libvorbis
           openal
           sdl2))
    (home-page "https://www.viewizard.com/")
    (synopsis "3D space shooter with spaceship upgrade possibilities")
    (description
     "Space is a vast area, an unbounded territory where it seems there is
a room for everybody, but reversal of fortune put things differently.  The
hordes of hostile creatures crawled out from the dark corners of the universe,
craving to conquer your homeland.  Their force is compelling, their legions
are interminable.  However, humans didn't give up without a final showdown and
put their best pilot to fight back.  These malicious invaders chose the wrong
galaxy to conquer and you are to prove it!  Go ahead and make alien aggressors
regret their insolence.")
    ;; Game is released under GPL3+ terms.  Artwork is subject to CC
    ;; BY-SA 4.0, and fonts to OFL1.1.
    (license (list license:gpl3+ license:cc-by-sa4.0 license:silofl1.1))))

(define-public barony
  (package
    (name "barony")
    (version "3.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TurningWheel/Barony")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y72k6zrqqhib3p05zkdklays2d218v51n87k7k68m0s7nnxa4vy"))
       ;; Fix textures for SDL 2.0.14.
       ;; See <https://github.com/TurningWheel/Barony/pull/582>.
       (patches (search-patches "barony-fix-textures.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DOPENAL_ENABLED=ON" ; enable sound
             "-DEDITOR_EXE_NAME=barony-editor") ; instead of generic "editor"
       #:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-installation
           (lambda _
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/lang")
                "${CMAKE_SOURCE_DIR}/lang")))))))
    (inputs
     (list glu
           libpng
           libvorbis
           openal
           physfs
           rapidjson
           sdl2
           sdl2-image
           sdl2-net
           sdl2-ttf
           zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://baronygame.com")
    (synopsis "3D first-person roguelike game")
    (description
     "Barony is a first-person roguelike role-playing game with cooperative
play.  The player must descend a dark dungeon and destroy an undead lich while
avoiding traps and fighting monsters.  The game features randomly generated
dungeons, 13 character classes, hundreds of items and artifacts, and
cooperative multiplayer for up to four players.  This package does @emph{not}
provide the game assets.")
    (license license:bsd-2)))

(define-public bastet
  (package
    (name "bastet")
    (version "0.43.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fph/bastet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09kamxapm9jw9przpsgjfg33n9k94bccv65w95dakj0br33a75wn"))
       (patches
        (search-patches "bastet-change-source-of-unordered_set.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "boost") "/include"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'check
           ;; The 'Test' target builds the tests, but doesn't actually run them.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "Test" make-flags)
             (setenv "HOME" ".")
             (invoke "./Test")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (share   (string-append out "/share"))
                    (hicolor (string-append share "/icons/hicolor")))
               (install-file "bastet"
                             (string-append out "/bin"))

               (install-file "bastet.desktop"
                             (string-append share "/applications"))
               (install-file "bastet.svg"
                             (string-append hicolor "/scalable/apps"))

               (install-file "bastet.appdata.xml"
                             (string-append share "/appdata"))

               (install-file "bastet.6"
                             (string-append out "/share/man/man6"))
               #t))))))
    (native-inputs
     (list hicolor-icon-theme))
    (inputs
     (list boost ncurses))
    (home-page "https://fph.altervista.org/prog/bastet.html")
    (synopsis "Antagonistic Tetris-style falling brick game for text terminals")
    (description
     "Bastet (short for Bastard Tetris) is a simple ncurses-based falling brick
game.  Unlike normal Tetris, Bastet does not choose the next brick at random.
Instead, it uses a special algorithm to choose the worst brick possible.

Playing bastet can be a painful experience, especially if you usually make
canyons and wait for the long I-shaped block to clear four rows at a time.")
    (license license:gpl3+)))

(define-public tetrinet
  (package
    (name "tetrinet")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://tetrinet.or.cz/download/tetrinet-" version
             ".tar.bz2"))
       (sha256
        (base32
         "0b4pddqz6is1771qmvcj8qqlr4in2djdbkk13agvp9yhfah2v8x7"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags '("CC=gcc"
                      "CFLAGS=-O2 -DHAVE_IPV6 -g -Wall -fcommon")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-after 'unpack 'fix-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (substitute* "Makefile"
                 (("/usr/games") (string-append out "/bin"))))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (lambda (file)
                           (install-file file doc))
                         (list "README" "tetrinet.txt"))))))))
    (home-page "http://tetrinet.or.cz")
    (synopsis "Terminal-based multiplayer Tetris clone")
    (description "Tetrinet is a multiplayer Tetris-like game with powerups and
attacks you can use on opponents.")
    (license license:public-domain)))

(define-public vdrift-data
  ;; There are no tags or releases for the vdrift data; use the latest SVN
  ;; revision available.
  (let ((commit 1463)
        (revision "0"))
    ;; The package is hidden as the game data is *required* by the install
    ;; target of vdrift itself, and there is no need for users to manually
    ;; install it.
    (hidden-package
     (package
       (name "vdrift-data")
       ;; The date is the last modified time shown next to the 'vdrift-data'
       ;; directory when visiting
       ;; https://sourceforge.net/p/vdrift/code/HEAD/tree/.
       (version (format #f "2024-10-23-~a.~a" revision commit))
       (source (origin
                 (method svn-fetch)
                 (uri (svn-reference
                       (url "https://svn.code.sf.net/p/vdrift/code/vdrift-data")
                       (revision commit)))
                 (file-name (string-append name "-" version "-checkout"))
                 (sha256
                  (base32
                   "1zx08q4v3s4l5r0wxphd323h0rqp9pjb7kr08s3gb2qr85lw587h"))))
       (build-system copy-build-system)
       (arguments (list #:install-plan #~'(("." "share/games/vdrift/data"))))
       (home-page "https://vdrift.net/")
       (synopsis "Game data for Vdrift")
       (description "This package contains the assets for the Vdrift racing
game.")
       (license license:gpl3+)))))      ;assumed same as Vdrift itself

(define-public vdrift
  ;; The latest release is from 2014, and lacks build system and other
  ;; unreleased improvements; use the latest commit.
  (let ((commit "120ae28d2a1b43a8589c5ce3c5e02d813890d090")
        (revision "0"))
    (package
      (name "vdrift")
      (version (git-version "2014-10-20" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/VDrift/vdrift")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13id01rr6rjhmrh34p8n0ka3yfwzp62j6p8z6rc5aagnr5mn1qn0"))))
      (build-system scons-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:scons-flags #~(list (string-append "prefix=" #$output)
                              "release=1"
                              "verbose=1")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'setup-vdrift-data
              (lambda _
                ;; The locale data must be made writable, as gettext
                ;; translation files are generated and written there as part
                ;; of the installation script.
                (copy-recursively (search-input-directory
                                   %build-inputs
                                   "share/games/vdrift/data")
                                  "data")
                (for-each make-file-writable (find-files "data/locale")))))))
      (native-inputs (list gettext-minimal pkg-config vdrift-data))
      (inputs (list bullet curl libvorbis mesa sdl2 zlib))
      (home-page "https://vdrift.net/")
      (synopsis "Racing simulator")
      (description "VDrift aims to provide an accurate driving physics
emulation, based on real world data of the actual vehicles, employing a full
rigid body simulation and a complex tire model.  VDrift features:
@itemize
@item Over 45 tracks based on famous real-world tracks
@item Over 45 cars based on real-world vehicles
@item Very realistic, simulation-grade driving physics
@item Mouse/joystick/gamepad/wheel/keyboard support
@item Fully modeled tracks, scenery and terrain
@item Several different camera modes
@item Basic replay system with Skip Forward/Skip Backward
@item Fully customizable controls
@item Joystick, mouse and keyboard input filtering
@item Brake and reverse lights
@item Driver aids: automatic shifting, traction control, anti-lock braking
@item Experimental force feedback
@item Race against up to 3 AI with variable difficultly
@item Engine and road sounds
@end itemize
The recommended input method is a steering wheel with pedals and force
feedback support.")
      (license license:gpl3+))))

(define-public vitetris
  (package
    (name "vitetris")
    (version "0.59.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vicgeralds/vitetris")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1ah1c5g7abksif0n8v5rb7r4pn2az20c3mkp4ak13vgs23ddmds5"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; the non standard configure script does not accept
             ;; standard parameters -> invoke configure by hand
             (invoke "./configure" "prefix=")
             ;; src/src-conf.mk must be writable for the build step
             (make-file-writable "src/src-conf.mk"))))))
    (home-page "http://victornils.net/tetris/")
    (synopsis "Terminal-based Tetris clone")
    (description "Vitetris is a classic multiplayer Tetris clone for the
terminal.")
    (license license:bsd-2)))

(define-public blobwars
  (package
    (name "blobwars")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/blobwars/"
                           "blobwars-" version ".tar.gz"))
       (sha256
        (base32 "16aagvkx6azf75gm5kaa94bh5npydvhqp3fvdqyfsanzdjgjf1n4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "BINDIR=" out "/bin/")
               "USEPAK=1"
               "RELEASE=1"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda _
             (substitute* "Makefile" (("-Werror") ""))
             ;; glibc 2.38 includes strlcpy and strlcat.
             (substitute* "src/headers.h"
               (("static inline void strlcat.*") "")
               (("static inline void strlcpy.*") ""))))
         (delete 'configure))))         ;no configure script
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("sdl" ,(sdl-union (list sdl2
                                sdl2-image
                                sdl2-mixer
                                sdl2-ttf
                                sdl2-net)))))
    (home-page "https://sourceforge.net/projects/blobwars/")
    (synopsis "Platform action game featuring a blob with a lot of weapons")
    (description "Blobwars: Metal Blob Solid is a 2D platform game, the first
in the Blobwars series.  You take on the role of a fearless Blob agent.  Your
mission is to infiltrate various enemy bases and rescue as many MIAs as
possible, while battling many vicious aliens.")
    (license (list license:gpl2      ; For code and graphics
                   license:cc0       ; Music and sounds have specific licenses
                   license:cc-by3.0  ; see /doc/readme
                   license:cc-by-sa3.0
                   license:lgpl2.1+
                   license:bsd-2))))

(define-public bsd-games
  (package
    (name "bsd-games")
    (version "2.17.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://ibiblio.org/pub/linux/games/bsd-games-2.17.tar.gz")
       (sha256
        (base32 "0q7zdyyfvn15y0w4g54kq3gza89h61py727m8slmw73cxx594vq6"))
       (patches
        (search-patches
         ;; thanks Arch, and Debian
         "bsd-games-2.17-64bit.patch"
         "bsd-games-bad-ntohl-cast.patch"
         "bsd-games-gamescreen.h.patch"
         "bsd-games-getline.patch"
         "bsd-games-null-check.patch"
         "bsd-games-number.c-and-test.patch"
         "bsd-games-stdio.h.patch"
         "bsd-games-prevent-name-collisions.patch"
         ;; Guix customizations
         "bsd-games-add-configure-config.patch"
         "bsd-games-dont-install-empty-files.patch"
         "bsd-games-add-wrapper.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list flex bison))
    (inputs
     `(("curses" ,ncurses)
       ("pager" ,less)
       ("miscfiles" ,miscfiles)
       ("openssl" ,openssl)))           ;used only by 'factor'
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/bsd-games-" ,version))
                    (man (string-append out "/share/man"))
                    (word-list (search-input-file inputs "/share/web2"))
                    (static-data (string-append out "/share/games/bsd-games"))
                    ;; Not a "./" because of substitute* in 'patch-install
                    ;; below.  The .// allow us not to mess with the games'
                    ;; code any further: we just use a wrapper script that
                    ;; cd's to a BSD_GAMES_DIR.  :]
                    (save-files ".//"))
               (substitute* "configure"
                 (("/usr/share/man") man)
                 (("/usr/share/doc/bsd-games") doc)
                 (("/usr/share/[^\n/]*") static-data)
                 (("/var/games") save-files)
                 (("/usr/bin/less") (which "less"))
                 (("(/usr/bin|/usr/games)") bin))
               (substitute* "config.params" (("WORD_LIST") word-list))
               (substitute* "wrapper" (("STATIC_DATA") static-data))
               (invoke "./configure"))
             #t))
         (add-before 'install 'patch-install
           ;; Some games need a writable directory containing pre-maded files.
           ;; The files get installed to the Store.  Then the wrapper kicks in.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (static-data (string-append out "/share/games/bsd-games"))
                    (save-files ".//"))
               (substitute* "Makeconfig" ((save-files) static-data)))
             #t))
         (add-after 'install 'install-documents
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/bsd-games-" ,version)))
               (rename-file "phantasia/COPYRIGHT" "phantasia-COPYRIGHT")
               (for-each
                (lambda(file) (install-file file doc))
                '("AUTHORS" "BUGS" "README" "SECURITY" "THANKS"
                  "phantasia-COPYRIGHT")))
             #t)))))
    (home-page "https://github.com/vattam/BSDGames")
    (synopsis "Collection of the old text-based games and amusements")
    (description
     "These are the BSD games.

Action: atc (keep the airplanes safe), hack (explore the dangerous Dungeon),
hunt (kill the others for the Pair of Boots, multi-player only), robots (avoid
the evil robots), sail (game of naval warfare with wooden ships), snake (steal
the $$ from the cave, anger the snake, and get out alive), tetris (game of
lining up the falling bricks of different shapes), and worm (eat, grow big,
and neither bite your tail, nor ram the wall).

Amusements: banner (prints a large banner), bcd & morse & ppt (print a punch
card, or paper tape, or Morse codes), caesar & rot13 (ciphers and deciphers
the input), factor (factorizes a number), number (translates numbers into
text), pig (translates from English to Pig Latin), pom (should print the
Moon's phase), primes (generates primes), rain & worms (plays an screen-saver
in terminal), random (prints randomly chosen lines from files, or returns a
random exit-code), and wtf (explains what do some acronyms mean).

Board: backgammon (lead the men out of board faster than the friend do),
boggle (find the words in the square of letters), dab (game of dots and
boxes), gomoku (game of five in a row), hangman (guess a word before man is
hanged), and monop (game of monopoly, hot-seat only).  Also the card-games:
canfield, cribbage, fish (juniors game), and mille.

Quests: adventure (search for treasures with the help of wizard),
battlestar (explore the world around, starting from dying spaceship),
phantasia (role-play as an rogue), trek (hunt the Klingons, and save the
Federation), and wump (hunt the big smelly Wumpus in a dark cave).

Quizzes: arithmetic and quiz.")
    ;; "Auxiliary and data files, distributed with the games in NetBSD, but
    ;; not bearing copyright notices, probably fall under the terms of the UCB
    ;; or NetBSD copyrights and licences.  The file "fortune/Notes" contains a
    ;; warning in regard to the fortune databases."
    (license (list
              ;; Most games.  Files: countmail/countmail.6, dab/dab.6,
              ;; lib/strlcpy.c, wargames/wargames.6
              license:bsd-3
              ;; dab and hunt.  Files: adventure/extern.h,
              ;; backgammon/backgammon/backlocal.h, caesar/rot13.in,
              ;; countmail/countmail, dm/utmpentry.c, dm/utmpentry.h,
              ;; hack/extern.h, robots/auto.c, sail/display.h,
              ;; sail/restart.h, wargames/wargames
              license:bsd-4
              ;; wtf (the game)
              license:public-domain
              ;; phantasia (all but phantasia/pathnames.h.in, which is bsd-3)
              (license:fsf-free "file:///phantasia/COPYRIGHT")))))

(define-public rogue
  (package
    (name "rogue")
    (version "5.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Davidslv/rogue")
                    (commit "cf9bd26d564a72fac4cf56b55c96c2435270d29a")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mk03l120scas4dcn6xccnhslnwmcx2blshbf925z06yk7rkzias"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "CFLAGS=-DNCURSES_INTERNALS")))
    (inputs (list ncurses))
    (synopsis "Original rogue game")
    (description
     "This package provides ``Rogue: Exploring the Dungeons of Doom'', the
original rogue game found on 4.2BSD.")
    (home-page "https://github.com/Davidslv/rogue")
    (license license:bsd-3)))

(define-public sgt-puzzles
  (let ((commit "50985e9f2c54ad44e8c26491ddddd698bc02fd06")
        (revision "0"))
    (package
      (name "sgt-puzzles")
      (version (git-version "20250510" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.tartarus.org/simon/puzzles.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j3bnzw4bbbm1nl9zmkmhcpk1zm64jmfjiymsjw8axzq5af19jvj"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;No tests.
        #:configure-flags #~(list "-DNAME_PREFIX=sgt-")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'set-xdg-open-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "gtk.c"
                  (("(#define HELP_BROWSER_PATH).+" all define)
                   (format #f "~a ~s~%" define
                           (search-input-file inputs "/bin/xdg-open")))))))))
      (inputs (list gtk+ xdg-utils))
      (native-inputs (list pkg-config perl imagemagick halibut))
      (home-page "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/")
      (synopsis "Simon Tatham's portable puzzle collection")
      (description "Simon Tatham's Portable Puzzle Collection contains a number of
popular puzzle games for one player.")
      (license license:expat))))

(define-public bzflag
  (package
    (name "bzflag")
    (version "2.4.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.bzflag.org/bzflag/source/"
                           version "/bzflag-" version ".tar.bz2"))
       (sha256
        (base32 "050h933lmcdf4bw9z3c6g3k8c9sch9f6kq57jp2ivb96zw2h90q1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-desktop-file-and-icons
            (lambda _
              (let* ((share (string-append #$output "/share"))
                     (data (string-append share "/bzflag"))
                     (hicolor (string-append share "/icons/hicolor"))
                     (applications (string-append share "/applications")))
                ;; Move desktop file.
                (install-file (string-append data "/bzflag.desktop")
                              applications)
                ;; Install icons.
                (for-each (lambda (size)
                            (let* ((dim (string-append size "x" size))
                                   (dir (string-append hicolor "/" dim "/apps")))
                              (mkdir-p dir)
                              (copy-file
                               (string-append data "/bzflag-" dim ".png")
                               (string-append dir "/bzflag.png"))))
                          '("48" "256"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list c-ares
           curl
           glew
           glu
           sdl2
           zlib))
    (home-page "https://www.bzflag.org/")
    (synopsis "3D first person tank battle game")
    (description
     "BZFlag is a 3D multi-player multiplatform tank battle game that
allows users to play against each other in a network environment.
There are five teams: red, green, blue, purple and rogue (rogue tanks
are black).  Destroying a player on another team scores a win, while
being destroyed or destroying a teammate scores a loss.  Rogues have
no teammates (not even other rogues), so they cannot shoot teammates
and they do not have a team score.

There are two main styles of play: capture-the-flag and free-for-all.
In capture-the-flag, each team (except rogues) has a team base and
each team with at least one player has a team flag.  The object is to
capture an enemy team's flag by bringing it to your team's base.  This
destroys every player on the captured team, subtracts one from that
team's score, and adds one to your team's score.  In free-for-all,
there are no team flags or team bases.  The object is simply to get as
high a score as possible.")
    ;; The game is dual-licensed.
    (license (list license:lgpl2.1 license:mpl2.0))))

(define-public cataclysm-dda
  (package
    (name "cataclysm-dda")
    (version "0.H")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CleverRaven/Cataclysm-DDA")
             (commit version)))
       (sha256
        (base32 "00lqpvr66h5bpkliln764nh7b0m6chs85yws1l6gg44mijkr6f1j"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              "USE_HOME_DIR=1" "DYNAMIC_LINKING=1" "RELEASE=1" "WARNINGS=-w"
              "LOCALIZE=1" "LANGUAGES=all")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          ;; Apparently we can't do make on both tiles and a console version at
          ;; the same time anymore, so we have to either "make clean" between
          ;; builds or do some other hackery.  See:
          ;;   https://github.com/CleverRaven/Cataclysm-DDA/issues/42598#issuecomment-667702746
          (add-after 'install 'make-clean-pre-tiles
            (lambda* (#:key make-flags outputs #:allow-other-keys)
              ;; Change prefix directory and enable tile graphics and sound.
              (invoke "make" "clean")))
          (add-after 'make-clean-pre-tiles 'build-tiles
            (lambda* (#:key make-flags outputs #:allow-other-keys)
              ;; Change prefix directory and enable tile graphics and sound.
              (apply invoke "make" "TILES=1" "SOUND=1"
                     (string-append "PREFIX=" #$output:tiles)
                     (cdr make-flags))))
          (add-after 'build-tiles 'install-tiles
            (lambda* (#:key make-flags outputs #:allow-other-keys)
              (apply invoke "make" "install" "TILES=1" "SOUND=1"
                     (string-append "PREFIX=" #$output:tiles)
                     (cdr make-flags)))))
      ;; TODO: Add libtap++ from https://github.com/cbab/libtappp as a native
      ;;       input in order to support tests.
      #:tests? #f))
    (outputs '("out"
               "tiles"))                ;for tile graphics and sound support
    (native-inputs
     (list astyle gettext-minimal pkg-config))
    (inputs
     (list freetype
           libogg
           libvorbis
           ncurses
           sdl2
           sdl2-image
           sdl2-ttf
           sdl2-mixer))
    (home-page "https://cataclysmdda.org/")
    (synopsis "Survival horror roguelike video game")
    (description
     "Cataclysm: Dark Days Ahead (or \"DDA\" for short) is a roguelike set
in a post-apocalyptic world.  Struggle to survive in a harsh, persistent,
procedurally generated world.  Scavenge the remnants of a dead civilization
for food, equipment, or, if you are lucky, a vehicle with a full tank of gas
to get you out of Dodge.  Fight to defeat or escape from a wide variety of
powerful monstrosities, from zombies to giant insects to killer robots and
things far stranger and deadlier, and against the others like yourself, that
want what you have.")
    (license license:cc-by-sa3.0)))

(define-public cockatrice
  (let ((release-date "2023-09-14"))
    (package
      (name "cockatrice")
      (version "2.9.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Cockatrice/Cockatrice")
               (commit (string-append release-date "-Release-" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jhn6pprd3j8m312mm8nlb9hwcchg3qkks5rw0x7d22alk922dlv"))
         (modules '((guix build utils)))
         (snippet
          ;; Strip image URLs as they point towards non-free web services
          '(substitute* "cockatrice/src/settings/downloadsettings.cpp"
             (("downloadURLs.append\\(\".*\"\\);") "")))))
      (build-system qt-build-system)
      (arguments
       `(#:configure-flags '("-DWITH_SERVER=1"
                             "-DWITH_CLIENT=1"
                             "-DWITH_ORACLE=1"
                             "-DTEST=1")))
      (native-inputs
       (list googletest pkg-config))
      (inputs
       (list protobuf
             qtbase-5
             qtmultimedia-5
             qtsvg-5
             qttools-5
             qtwebsockets-5
             qtwayland-5
             xz
             zlib))
      (home-page "https://cockatrice.github.io")
      (synopsis "Tabletop card game simulator")
      (description "Cockatrice is a program for playing tabletop card games
over a network.  Its server design prevents users from manipulating the game
for unfair advantage.  The client also provides a single-player mode, which
allows users to brew while offline.")
      (license license:gpl2))))

(define-public corsix-th
  (package
    (name "corsix-th")
    (version "0.67")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CorsixTH/CorsixTH")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14996kbrwdrd0gpz19il2i4p650qdhjw8v8ka3aigk6pl4kda3sq"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-binary
            (lambda _
              ;; Set Lua module paths and default MIDI soundfont on startup.
              (let* ((fluid #$(this-package-input "fluid-3"))
                     (lua-version #$(version-major+minor (package-version lua)))
                     (lua-cpath
                      (map (lambda (lib)
                             (string-append
                              (assoc-ref %build-inputs (string-append "lua-" lib))
                              "/lib/lua/" lua-version "/?.so"))
                           '("filesystem" "lpeg"))))
                (wrap-program (string-append #$output "/bin/corsix-th")
                  `("LUA_CPATH" ";" = ,lua-cpath)
                  `("SDL_SOUNDFONTS" ":" suffix
                    (,(string-append fluid "/share/soundfonts/FluidR3Mono_GM.sf3"))))))))
      #:tests? #f)) ; TODO need busted package to run tests
    ;; Omit Lua-Socket dependency to disable automatic updates.
    (inputs
     (list bash-minimal
           ffmpeg
           fluid-3
           freetype
           lua
           lua-filesystem
           lua-lpeg
           sdl2
           sdl2-mixer))
    (home-page "https://corsixth.com")
    (synopsis "Implementation of the @i{Theme Hospital} game engine")
    (description
     "This package provides a reimplementation of the 1997 Bullfrog business
simulation game @i{Theme Hospital}.  As well as faithfully recreating the
original engine, CorsixTH adds support for high resolutions, custom levels and
more.  This package does @emph{not} provide the game assets.")
    (license (list
              license:expat ; main license
              license:bsd-3)))) ; CorsixTH/Src/random.c

(define-public cowsay
  ;; This is a continuation of Tony Monroe's now-unmaintained original, that
  ;; aims to become the ‘canonical modern fork’.  We'll see.  What it gives
  ;; us today is a bunch of fixes that other distros shipped as patches.
  (package
    (name "cowsay")
    (version "3.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cowsay-org/cowsay")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xdrpqj0lf3x1aib4s1bqfq4p7dxxlw1560pp1kw6pk3mzyvxih5"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "prefix=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key outputs #:allow-other-keys)
                   (invoke (string-append (assoc-ref outputs "out")
                                          "/bin/cowsay")
                           "We're done!"))))))
    (inputs
     (list perl))
    (home-page (string-append "https://web.archive.org/web/20071026043648/"
                              "http://www.nog.net:80/~tony/warez/cowsay.shtml"))
    (synopsis "Speaking cow text filter")
    (description "Cowsay is basically a text filter.  Send some text into it,
and you get a cow saying your text.  If you think a talking cow isn't enough,
cows can think too: all you have to do is run @command{cowthink}.  If you're
tired of cows, a variety of other ASCII-art messengers are available.")
    (license license:gpl3+)))

(define-public deal
  (package
    (name "deal")
    (version "3.1.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gtwilliams/deal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wcrx3yq5ycnkdnygcq80ljpgc9iwyrr8zayprzvbibvj77hdm0c"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(let* ((tcl #$(this-package-input "tcl"))
               (tcl-version #$(version-major+minor (package-version tcl))))
          (list "CPPFLAGS += -O3"
                (string-append "CC=" #$(cc-for-target))
                (string-append "TCL_INCL=" tcl "/include")
                (string-append "LDFLAGS=-L" tcl "/lib"
                               " -ltcl" tcl-version
                               " -lm")
                (string-append "DATA_DIR=" #$output "/share/deal/")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'locate-pod2man
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "Makefile"
                (("/usr/bin/pod2man")
                 (search-input-file inputs "/bin/pod2man")))))
          (delete 'configure)           ;no configure script
          ;; Prevent the error: "Makefile:248: Make.dep: No such file
          ;; or directory".
          (add-before 'build 'create-Make.dep
            (lambda _
              (call-with-output-file "Make.dep" (const #t))))
          ;; There is no install target.  Do everything manually.
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (man (string-append #$output "/share/man/man6"))
                    (data (string-append #$output "/share/deal")))
                (install-file "deal" bin)
                (install-file "deal.6" man)
                (install-file "deal.tcl" data)
                (for-each (lambda (d)
                            (let ((target (string-append data "/" d)))
                              (mkdir-p target)
                              (copy-recursively d target)))
                          '("ex" "format" "input" "lib")))))
          ;; Tests need to happen once the data is properly installed
          ;; because the "deal.tcl" script file location is hard-coded
          ;; in the "deal" binary.
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check)))))
    (native-inputs
     (list perl))
    (inputs
     (list tcl))
    (home-page "https://bridge.thomasoandrews.com/deal/")
    (synopsis "Bridge hand generator")
    (description
     "This program generates bridge hands.  It can be told to generate only
hands satisfying conditions like being balanced, having a range of
High Cards Points (HCP), controls, or other user-definable properties.
Hands can be output in various formats, like PBN for feeding to other
bridge programs, Deal itself, or split up into a file per player for
practise.")
    (license (list license:gpl2+
                   license:gpl1+        ;ansidecl.h
                   license:bsd-3))))    ;random.c

(define-public doom-runner
  (package
    (name "doom-runner")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Youda008/DoomRunner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rpywq95zy9w0wj1262x4rf84c52wg1rgf0by549qph6fybn34rn"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (substitute* "DoomRunner.pro"
                (("/usr")
                 #$output))
              (invoke "qmake" "DoomRunner.pro" "-spec" "linux-g++"
                      "\"CONFIG+=release\"")))
          (add-after 'install 'install-xdg
            (lambda _
              (with-directory-excursion "Install/XDG"
                (install-file "DoomRunner.desktop"
                              (string-append #$output
                                             "/share/applications"))
                (let ((install-icon
                       (lambda (size)
                         (install-file (simple-format
                                        #f "DoomRunner.~sx~s.png"
                                        size size)
                                       (simple-format
                                        #f "~a/share/icons/hicolor/~sx~s/apps"
                                        #$output size size)))))
                  (for-each install-icon
                            '(16 24 32 48 64 128)))))))))
    (home-page "https://github.com/Youda008/DoomRunner")
    (synopsis "Launcher for Doom engine games")
    (description
     "Doom Runner is yet another launcher of common Doom source ports (like
GZDoom, Zandronum, PrBoom, ...) with graphical user interface.  It is
written in C++ and Qt, and it is designed around the idea of presets
for various multi-file modifications to allow one-click switching
between them and minimize any repetitive work.")
    (license license:gpl3)))

(define-public falltergeist
  (package
    (name "falltergeist")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/falltergeist/falltergeist")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05cg58i2g32wbmrvmdsicic8xs83gld3qr1p7r4lnlckcl1l7dy4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests provided
    (native-inputs (list pkg-config))
    (inputs `(("sdl" ,(sdl-union (list sdl2
                                       sdl2-image
                                       sdl2-mixer)))
              ("glew" ,glew)
              ("glm" ,glm)))
    (home-page "https://falltergeist.org/")
    (synopsis "Fallout 2 game engine")
    (description "This package provides the Fallout 2 game engine.  Game data
should be placed in @file{~/.local/share/falltergeist}.")
    (license license:gpl3+)))

(define-public foobillard++
  ;; Even though this latest revision is old already, stable release is
  ;; lagging way behind it, and has issues with textures rendering.
  (let ((svn-revision 170))
    (package
      (name "foobillard++")
      (version (string-append "3.43-r" (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.code.sf.net/p/foobillardplus/code/")
               (revision svn-revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00b693ys5zvzjbjzzj3dqfzm5xw64gwjf9m8qv6bkmf0klbhmayk"))
         (patches
          (search-patches "foobillard++-pkg-config.patch"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Unfortunately, the game includes background music with
             ;; a non-commercial clause.  Delete it.
             (for-each delete-file (find-files "data/music" "\\.ogg$"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list
          ;; Install data in a less exotic location.
          (string-append "--prefix=" (assoc-ref %outputs "out") "/share")
          ;; Prevent a build error about undefined trigonometric functions.
          "--enable-fastmath=no")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-makefile
             ;; Remove hard-coded directories.  Also fix installation
             ;; rule: it tries to move around non-existent files or
             ;; files already moved.
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "Makefile.am"
                 (("/usr") (assoc-ref outputs "out"))
                 (("cp .*?/foobillardplus\\.desktop.*") "")
                 (("cp .*?/foobillardplus\\.(png|xbm) \\$\\(datarootdir\\).*")
                  ""))
               #t))
           (add-after 'unpack 'unbundle-font
             ;; XXX: The package ships with LinBiolinum_aSB.ttf and
             ;; LinBiolinum_aS.ttf, which are not provided by
             ;; `font-linuxlibertine' package.  Therefore, we cannot replace
             ;; them yet.
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((dejavu (string-append (assoc-ref inputs "font-dejavu")
                                            "/share/fonts/truetype/")))
                 (with-directory-excursion "data"
                   (for-each (lambda (f)
                               (delete-file f)
                               (symlink (string-append dejavu f) f))
                             '("DejaVuSans-Bold.ttf" "DejaVuSans.ttf"))))
               #t))
           (replace 'bootstrap
             (lambda _
               (invoke "aclocal" "--force")
               (invoke "autoconf" "-f")
               (invoke "autoheader" "-f")
               (invoke "automake" "-a" "-c" "-f")))
           (add-before 'build 'prepare-build
             ;; Set correct environment for SDL.
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (search-input-directory inputs "include/SDL")
                        ":" (or (getenv "CPATH") "")))))
           (add-before 'build 'fix-settings-directory
             ;; Hide foobillardplus settings directory in $HOME.
             (lambda _
               (substitute* "src/history.c"
                 (("/foobillardplus-data") "/.foobillardplus"))
               #t))
           (add-before 'install 'create-directories
             ;; Install process does not create directories before
             ;; trying to move file in it.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/share/icons"))
                 (mkdir-p (string-append out "/share/applications")))
               #t))
           (add-after 'install 'symlink-executable
             ;; Symlink executable to $out/bin.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (with-directory-excursion bin
                   (symlink "../share/foobillardplus/bin/foobillardplus"
                            "foobillardplus"))
                 #t))))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       `(("font-dejavu" ,font-dejavu)
         ("freetype" ,freetype)
         ("glu" ,glu)
         ("libpng" ,libpng)
         ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-net)))))
      (home-page "https://foobillardplus.sourceforge.net/")
      (synopsis "3D billiard game")
      (description "FooBillard++ is an advanced 3D OpenGL billiard game
based on the original foobillard 3.0a sources from Florian Berger.
You can play it with one or two players or against the computer.

The game features:

@itemize
@item Wood paneled table with gold covers and gold diamonds.
@item Reflections on balls.
@item Zoom in and out, rotation, different angles and bird's eye view.
@item Different game modes: 8 or 9-ball, Snooker or Carambole.
@item Tournaments.  Compete against other players.
@item Animated cue with strength and eccentric hit adjustment.
@item Jump shots and snipping.
@item Realistic gameplay and billiard sounds.
@item Red-Green stereo.
@item And much more.
@end itemize")
      (license (list license:gpl2 license:silofl1.1)))))

(define-public freedoom
  (package
    (name "freedoom")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freedoom/freedoom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01fwzwi4a68n26d627dkcn85jz854mc3zfnzzkvinmx9yy3z5qmq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((freedoom (assoc-ref outputs "out"))
                    (wad-dir (string-append freedoom "/share/games/doom")))
               ;; Make sure that the install scripts know where to find
               ;; the appropriate WAD files.
               (substitute* "dist/freedoom"
                 (("IWAD=freedm.wad")
                  (string-append "IWAD=" wad-dir "/freedm.wad"))
                 (("IWAD=freedoom1.wad")
                  (string-append "IWAD=" wad-dir "/freedoom1.wad"))
                 (("IWAD=freedoom2.wad")
                  (string-append "IWAD=" wad-dir "/freedoom2.wad")))
               #t))))))
    (native-inputs
     (list asciidoc deutex python python-pillow))
    (home-page "https://freedoom.github.io/")
    (synopsis "Free content game based on the Doom engine")
    (native-search-paths
     (list (search-path-specification
            (variable "DOOMWADDIR")
            (files '("share/games/doom")))
           (search-path-specification
            (variable "DOOMWADPATH")
            (files '("share/games/doom")))))
    (description
     "The Freedoom project aims to create a complete free content first person
shooter game.  Freedoom by itself is just the raw material for a game: it must
be paired with a compatible game engine (such as @code{prboom-plus}) to be
played.  Freedoom complements the Doom engine with free levels, artwork, sound
effects and music to make a completely free game.")
    (license license:bsd-3)))

(define-public freedroidrpg
  (package
    (name "freedroidrpg")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (let ((major+minor
                   (version-major+minor
                    (string-replace-substring version "rc" "."))))
              (string-append "http://ftp.osuosl.org/pub/freedroid/"
                             "freedroidRPG-" major+minor "/"
                             "freedroidRPG-" version ".tar.gz")))
       (sha256
        (base32 "1kxvyg70r9x8q40kn5lr3h1q60d6jx9mkvxls4aflj22b45vg5br"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         (string-append "CFLAGS=-fcommon "
                        "-I" #$(this-package-input "sdl-gfx") "/include/SDL "
                        "-I" #$(this-package-input "sdl-image") "/include/SDL "
                        "-I" #$(this-package-input "sdl-mixer") "/include/SDL")
         "--enable-opengl")
      ;; FIXME: the test suite fails with the following error output:
      ;;   4586 Segmentation fault      env SDL_VIDEODRIVER=dummy \
      ;;   SDL_AUDIODRIVER=dummy ./src/freedroidRPG -nb text
      #:tests? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glew
           glu
           libjpeg-turbo
           libogg
           libpng
           libvorbis
           mesa
           python-wrapper
           sdl
           sdl-gfx
           sdl-image
           sdl-mixer
           zlib))
    (home-page "https://www.freedroid.org/")
    (synopsis "Isometric role-playing game against killer robots")
    (description
     "Freedroid RPG is an @dfn{RPG} (Role-Playing Game) with isometric graphics.
The game tells the story of a world destroyed by a conflict between robots and
their human masters.  To restore peace to humankind, the player must complete
numerous quests while fighting off rebelling robots---either by taking control
of them, or by simply blasting them to pieces with melee and ranged weapons in
real-time combat.")
    (license (list license:expat        ; lua/
                   license:gpl3         ; src/gen_savestruct.py
                   license:gpl2+))))    ; the rest

(define-public golly
  (package
    (name "golly")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/golly/golly/golly-"
                                  version "/golly-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "0pg9cp83nxc354lizgza5bqdy7z5wh36863203zw6r6s4flji4an"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "GOLLYDIR="
                                         (assoc-ref %outputs "out")
                                         "/share/golly"))
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (with-directory-excursion "gui-wx"
               (apply invoke `("make" ,@make-flags "-f" "makefile-gtk")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/golly"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (share (string-append out "/share/golly")))
               (for-each (lambda (binary)
                           (install-file binary bin))
                         '("bgolly" "golly"))
               (for-each (lambda (document)
                           (install-file
                            (string-append "docs/" document ".html")
                            doc))
                         '("License" "ReadMe" "ToDo"))
               (install-file "gui-wx/icons/appicon.xpm" pixmaps)
               (for-each (lambda (folder)
                           (copy-recursively
                            folder
                            (string-append share "/" folder)))
                         '("Help" "Patterns" "Rules" "Scripts")))
             #t)))))
    (native-inputs
     (list lua))
    (inputs
     (list glu mesa python sdl2 wxwidgets zlib))
    (home-page "https://golly.sourceforge.net/")
    (synopsis "Software for exploring cellular automata")
    (description
     "Golly simulates Conway's Game of Life and many other types of cellular
automata.  The following features are available:
@enumerate
@item Support for bounded and unbounded universes, with cells of up to 256
  states.
@item Support for multiple algorithms, including Bill Gosper's Hashlife
  algorithm.
@item Loading patterns from BMP, PNG, GIF and TIFF image files.
@item Reading RLE, macrocell, Life 1.05/1.06, dblife and MCell files.
@item Scriptable via Lua or Python.
@item Extracting patterns, rules and scripts from zip files.
@item Downloading patterns, rules and scripts from online archives.
@item Pasting patterns from the clipboard.
@item Unlimited undo/redo.
@item Configurable keyboard shortcuts.
@item Auto fit option to keep patterns within the view.
@end enumerate")
    (license license:gpl2+)))

(define-public joycond
  ;; Important: Do *not* update to a newer commit until an issue causing a
  ;; 100% CPU usage in the next commit
  ;; cdec32865c6093bd4761326ea461aaa2fcf7d1b4 is resolved (see:
  ;; https://github.com/DanielOgorchock/joycond/issues/153).
  (let ((commit "5b590ecc9bca181d8bc21377e752126bc9180319")
        (revision "2"))
    (package
      (name "joycond")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DanielOgorchock/joycond")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18aig7x5n8hh8ffw0qk2hx2b93xrs4jp652vp8cxklw9xyy33f15"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                ;no test suite
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-bin-location
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "CMakeLists.txt"
                       (("/lib/udev/rules.d")
                        (string-append #$output "/lib/udev/rules.d"))
                       (("/etc/systemd/system")
                        (string-append #$output "/etc/systemd/system"))
                       (("/etc/modules-load.d")
                        (string-append #$output "/etc/modules-load.d"))
                       (("/usr/bin")
                        (string-append #$output "/bin")))
                     (substitute* "udev/89-joycond.rules"
                       (("/bin/setfacl")
                        (search-input-file inputs "bin/setfacl"))))))))
      (native-inputs (list pkg-config))
      (inputs (list acl eudev libevdev))
      (home-page "https://github.com/DanielOgorchock/joycond")
      (synopsis "Joy-Con controller daemon")
      (description "This package provides a userspace daemon for the Nintendo
Joy-Con controllers.")
      (license license:gpl3+))))

(define-public julius
  (package
    (name "julius")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bvschaik/julius")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w7kmgz9ya0ck9cxhsyralarg7y6ydx4plmh33r4mkxkamlr7493"))
       ;; Remove unused bundled libraries.
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "ext"
             (for-each delete-file-recursively '("dirent" "png" "SDL2" "zlib")))
           #t))))
    (build-system cmake-build-system)
    (inputs
     (list libpng sdl2 sdl2-mixer))
    (home-page "https://github.com/bvschaik/julius")
    (synopsis "Re-implementation of Caesar III game engine")
    (description
     "Engine for Caesar III, a city-building real-time strategy game.
Julius includes some UI enhancements while preserving the logic (including
bugs) of the original game, so that saved games are compatible.  This package
does not include game data.")
    (license (list license:agpl3
                   license:zlib))))     ; ext/tinyfiledialogs

(define-public augustus
  (package
    (inherit julius)
    (name "augustus")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Keriew/augustus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d1k5279imc17mk3lxn8amc4ljgcj4v6x6lj2w3bph1z0a7a4bim"))
       ;; Remove unused bundled libraries.
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "ext"
             (for-each delete-file-recursively
                       '("dirent" "expat" "png" "SDL2" "zlib")))))))
    (arguments
     ;; No tests.  See https://github.com/Keriew/augustus/issues/82.
     `(#:tests? #f))
    (inputs (modify-inputs (package-inputs julius)
              (prepend expat)))
    (home-page "https://github.com/Keriew/augustus")
    (synopsis "Re-implementation of Caesar III game engine with gameplay changes")
    (description
     "Fork of Julius, an engine for the a city-building real-time strategy
game Caesar III.  Gameplay enhancements include:

@itemize
@item roadblocks;
@item market special orders;
@item global labour pool;
@item partial warehouse storage;
@item increased game limits;
@item zoom controls.
@end itemize\n")))

(define-public meandmyshadow
  (package
    (name "meandmyshadow")
    (version "0.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/meandmyshadow/"
                                  version "/meandmyshadow-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "0i98v6cgmpsxy7mbb0s2y6f6qq6mkwzk2nrv1nz39ncf948aky2h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (native-inputs
     (list pkg-config))
    (inputs
     `(("curl" ,curl)
       ("libarchive" ,libarchive)
       ("lua" ,lua)
       ("sdl" ,(sdl-union (list sdl2
                                sdl2-image
                                sdl2-mixer
                                sdl2-ttf)))))
    (home-page "https://acmepjz.github.io/meandmyshadow/")
    (synopsis "Puzzle/platform game")
    (description "Me and My Shadow is a puzzle/platform game in which you try
to reach the exit by solving puzzles.  Spikes, moving blocks, fragile blocks
and much more stand between you and the exit.  Record your moves and let your
shadow mimic them to reach blocks you couldn't reach alone.")
    (license license:gpl3+)))

(define-public opensurge
  (package
    (name "opensurge")
    (version "0.6.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alemart/opensurge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hwnjgkbywspmsmpmmnndqil86qqyd21y2q5krs8znwi35ychd3q"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ; there are no tests
           #:configure-flags
           #~(list (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                   (string-append "-DGAME_BINDIR=" #$output "/bin") ; not games
                   (string-append "-DGAME_DATADIR=" #$output "/share/" #$name)
                   (string-append "-DDESKTOP_ENTRY_PATH=" #$output "/share/applications")
                   (string-append "-DDESKTOP_ICON_PATH=" #$output "/share/pixmaps")
                   (string-append "-DDESKTOP_METAINFO_PATH=" #$output "/share/metainfo"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-xdg-open-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Look for xdg-open in the store.
                   (substitute* "src/core/web.c"
                     (("/usr/(bin/xdg-open)" _ bin)
                      (search-input-file inputs bin)))))
               (add-after 'unpack 'unbundle-fonts
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Replace bundled fonts with links to the store.
                   (with-directory-excursion "fonts"
                     (for-each (lambda (font)
                                 (let ((file (string-append "share/fonts/truetype/"
                                                            font)))
                                   (delete-file font)
                                   (symlink (search-input-file inputs file) font)))
                               '("Roboto-Black.ttf"
                                 "Roboto-Bold.ttf"
                                 "Roboto-Medium.ttf"))))))))
    (inputs
     (list allegro font-google-roboto mesa physfs surgescript xdg-utils))
    (home-page "https://opensurge2d.org")
    (synopsis "2D retro side-scrolling game")
    (description "@code{Open Surge} is a 2D retro side-scrolling platformer
inspired by the Sonic games.  The player runs at high speeds through each
level while collecting items and avoiding obstacles.  The game includes a
built-in level editor.")
    (license
     ;; Code is under GPL 3+, assets are under various licenses.
     ;; See src/misc/credits.c for details.
     (list license:gpl3+
           license:cc0
           license:cc-by3.0
           license:cc-by-sa3.0
           license:expat
           license:public-domain
           license:silofl1.1))))

(define-public kgames
  (package
    (name "kgames")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keith-packard/kgames")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09alaszhwnzzv5b0cxv5dia9y0fm63n2igv42ydkq299h7avb5fq"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~'("-Duser-menu=false"
               "-Dbindir=bin")))
    (native-inputs
     (list bison flex pkg-config))
    (inputs
     (list cairo
           fontconfig
           freetype
           librsvg
           libx11
           libxaw
           libxft
           libxmu
           libxpm
           libxrender
           ncurses))
    (synopsis "Xaw based solitaire games")
    (home-page "https://github.com/keith-packard/kgames")
    (description
     "This package provides a collection of solitaire games: kaces, kcanfield,
kcribbage, kdominos, kklondike, kmcarlo, kmontana, kslyfox, kspider, ktabby,
kthieves, ktowers, xmille and xreversi.")
    ;; Code is under BSD-3 and Expat, card images are under CC0.
    (license (list license:bsd-3 license:expat license:cc0))))

(define-public knightsgame
  (package
    (name "knightsgame")
    (version "025")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.knightsgame.org.uk/files/knights_"
                                  version "_src.tar.gz"))
              (sha256
               (base32
                "18vp2ygvn0s0jz8rm585jqf6hjqkam1ximq81k0r9hpmfj7wb88f"))
              (modules '((guix build utils)))
              (snippet
               ;; Fix a missing include for std::map.
               #~(substitute* "src/shared/impl/lua_func_wrapper.cpp"
                   (("#include \"misc[.]hpp\"" x)
                    (string-append "#include <map>\n" x))))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CXXFLAGS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         ;; No configure script.
         (delete 'configure))
       #:tests? #f)) ;; No check target.
    (inputs
     `(("boost" ,boost)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("curl" ,curl)))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.knightsgame.org.uk/")
    (synopsis "Multiplayer dungeon game involving knights and quests")
    (description "Knights is a multiplayer game involving several knights who
must run around a dungeon and complete various quests.  Each game revolves
around a quest – for example, you might have to find some items and carry them
back to your starting point.  This may sound easy, but as there are only
enough items in the dungeon for one player to win, you may end up having to
kill your opponents to get their stuff!  Other quests involve escaping from
the dungeon, fighting a duel to the death against the enemy knights, or
destroying an ancient book using a special wand.")
    ;; This package includes modified sources of lua (X11), enet (Expat), and
    ;; guichan (BSD-3).  The "Coercri" library is released under the Boost
    ;; license.  The whole package is released under GPLv3+.
    (license license:gpl3+)))

(define-public ghosthop
  (let ((commit "9fefc22830ff8fde484452d7a249f4c54feec0fc")
        (revision "1"))
    (package
      (name "ghosthop")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/gcmas/ghosthop.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "021awll73bgjk61r6y6cbqn0dpmki9jp627km7yhx5px15panslq"))
         (modules '((guix build utils)))
         (snippet '(delete-file-recursively "vendor"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        ;; 'ghosthop' loads 'asset' and 'scm' from the current directory.
        #~'(("ghosthop" "opt/ghosthop/")
            ("asset" "opt/ghosthop/")
            ("scm" "opt/ghosthop/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'build
              ;; Avoid its Makefile to build with system libraries.
              (lambda* (#:key inputs #:allow-other-keys)
                (with-output-to-file "rlbind.c"
                  (lambda ()
                    (invoke "repl" "rlbind.scm")))
                (invoke #$(cc-for-target)
                        "-o" "ghosthop"
                        "-O3" "-lm" "-lraylib"
                        (search-input-file inputs "/share/s7/s7.c")
                        "rlbind.c" "src/init.c")
                (invoke #$(strip-for-target) "ghosthop")))
            (add-after 'install 'install-launcher
              ;; Create a launcher script for 'ghosthop'.
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bindir (string-append out "/bin"))
                       (gamedir (string-append out "/opt/ghosthop"))
                       (launcher (string-append bindir "/ghosthop"))
                       (bash (search-input-file inputs "/bin/bash")))
                  (mkdir (dirname launcher))
                  (with-output-to-file launcher
                    (lambda ()
                      (format #t "#!~a~%" bash)
                      (format #t "cd ~a~%" gamedir)
                      (format #t "exec -a ghosthop ~a~%"
                              (string-append gamedir "/ghosthop"))))
                  (chmod launcher #o755)))))))
      (native-inputs (list s7))
      (inputs (list bash-minimal raylib))
      (home-page "https://gitlab.com/gcmas/ghosthop")
      (synopsis "Puzzle game with colored rules")
      (description "This is a small puzzle game made for the 2024 Spring Lisp
Game Jam.  The objective is to reach the goal by assigning rules to colors.")
      (license (list license:gpl3+            ;code and other assets
                     license:cc-by-sa3.0))))) ;music

(define-public gnome-2048
  (package
    (name "gnome-2048")
    (version "3.38.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-2048/"
                                  (version-major+minor version)  "/"
                                  "gnome-2048-" version ".tar.xz"))
              (sha256
               (base32
                "0s5fg4z5in1h39fcr69j1qc5ynmg7a8mfprk3mc3c0csq3snfwz2"))
              (patches
               (search-patches "gnome-2048-fix-positional-argument.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true")))))))
    (inputs
     (list gtk+
           clutter
           clutter-gtk
           libgee
           libgnome-games-support-1))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")       ; for desktop-file-validate and appstream-util
           itstool
           libxml2
           pkg-config
           vala))
    (home-page "https://wiki.gnome.org/Apps/2048")
    (synopsis "Move the tiles until you obtain the 2048 tile")
    (description "GNOME 2048 provides a 2D grid for playing 2048, a
single-player sliding tile puzzle game.  The objective of the game is to merge
together adjacent tiles of the same number until the sum of 2048 is achieved
in one tile.")
    (license license:gpl3+)))

(define-public gnome-chess
  (package
    (name "gnome-chess")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-chess/"
                                  (version-major version)  "/"
                                  "gnome-chess-" version ".tar.xz"))
              (sha256
               (base32
                "1rzx8qxrfsicdmkyka434nv7adrh4x4qn6dri5bjqcallzh91g52"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("update_desktop_database: true")
                "update_desktop_database: false")))))))
    (inputs
     (list gtk libadwaita librsvg))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")       ; for desktop-file-validate and appstream-util
           itstool
           pkg-config
           vala))
    (home-page "https://wiki.gnome.org/Apps/Chess")
    (synopsis "Chess board for GNOME")
    (description "GNOME Chess provides a 2D board for playing chess games
against human or computer players.  It supports loading and saving games in
Portable Game Notation.  To play against a computer, install a chess engine
such as chess or stockfish.")
    (license license:gpl3+)))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.07.001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnubg/gnubg-release-"
                           version "-sources.tar.gz"))
       (sha256
        (base32
         "07l2srlm05c99l4pppba8l54bnh000ns2rih5h8rzbcw84lrffbj"))))
    (build-system gnu-build-system)
    (inputs (list ;; XXX: Build with an older Pango for 'pango_font_get_hb_font' and
                  ;; 'pango_coverage_get_type'.  Try removing this for versions > 1.07.001.
                  pango-1.42
                  glib
                  readline
                  gtk+-2
                  mesa
                  glu
                  gtkglext
                  sqlite
                  libcanberra
                  libxcrypt))           ;required by Python.h
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       ;; SSE instructions are available on Intel systems only.
       (list ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                    (%current-system)))
                        '("x86_64" "i686"))
                   '("--enable-simd=sse2") ; prevent avx instructions
                   '()))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file (string-append apps "/gnubg.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                            Name=GNU Backgammon~@
                            Exec=~a/bin/gnubg -w~@
                            Icon=gnubg~@
                            Categories=Game;~@
                            Terminal=false~@
                            Type=Application~%"
                           out))))
             #t)))))
    (home-page "https://www.gnu.org/software/gnubg/")
    (synopsis "Backgammon game")
    (description "The GNU backgammon application (also known as \"gnubg\") can
be used for playing, analyzing and teaching the game.  It has an advanced
evaluation engine based on artificial neural networks suitable for both
beginners and advanced players.  In addition to a command-line interface, it
also features an attractive, 3D representation of the playing board.")
    (license license:gpl3+)))

(define-public gnubik
  (package
    (name "gnubik")
    (version "2.4.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1vlf924mq8hg93bsjj0rzvs0crc6psmlxyc6zn0fr7msnmpx6gib"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'skip-gtk-update-icon-cache
                    (lambda _
                      ;; Do not attempt to run 'gtk-update-icon-cache', which is
                      ;; unnecessary and causes a needless dependency on glib.
                      (substitute* "Makefile.in"
                        (("gtk-update-icon-cache")
                         "true"))
                      #t)))))
    (inputs (list gtk+-2
                  mesa
                  glu
                  libx11
                  guile-2.0
                  gtkglext))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gnubik/")
    (synopsis "3d Rubik's cube game")
    (description
     "GNUbik is a puzzle game in which you must manipulate a cube to make
each of its faces have a uniform color.  The game is customizable, allowing
you to set the size of the cube (the default is 3x3) or to change the colors.
You may even apply photos to the faces instead of colors.  The game is
scriptable with Guile.")
    (license license:gpl3+)))

(define-public gnushogi
  (package
    (name "gnushogi")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnushogi/gnushogi-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0a9bsl2nbnb138lq0h14jfc5xvz7hpb2bcsj4mjn6g1hcsl4ik0y"))
              (modules '((guix build utils)))
              ;; Fix "warning: ISO C90 does not support ‘__func__’ predefined
              ;; identifier [-Wpedantic]"
              (snippet '(begin
                          (substitute* "gnushogi/dspwrappers.c"
                            (("__FUNCTION__")
                             "__extension__ __FUNCTION__"))))))
    (arguments
     `(#:configure-flags (list (string-append
                                "CFLAGS="
                                (string-join '("-Wno-format"
                                               "-Wno-unused-but-set-variable"
                                               "-Wno-bool-compare")
                                             " ")))
       #:make-flags '("LDFLAGS=-z muldefs")
       #:phases (modify-phases %standard-phases
                  ;; Skip --enable-fast-install flag
                  (replace 'configure
                    (lambda* (#:key outputs configure-flags #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CONFIG_SHELL"
                                (which "sh"))
                        (setenv "SHELL"
                                (which "sh"))
                        (apply invoke "./configure"
                               (string-append "--prefix=" out) configure-flags)))))
       #:test-target "sizetest"))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "Game of Shogi (Japanese chess)")
    (description
     "GNU Shogi is a program that plays the game Shogi (Japanese Chess).
It is similar to standard chess but this variant is far more complicated.")
    (license license:gpl3+)))

(define-public ltris
  (package
    (name "ltris")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lgames/ltris/"
                           "ltris-" version ".tar.gz"))
       (sha256
        (base32 "144zvnnky79z5ychyyb2wsp7h2pcbl50fbzd9w9dvxkw6adz4yip"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "include/SDL")
                      ":" (or (getenv "CPATH") ""))))))))
    (inputs
     (list (sdl-union (list sdl sdl-mixer))))
    (home-page "https://lgames.sourceforge.net/LTris/")
    (synopsis "Tetris clone based on the SDL library")
    (description
     "LTris is a tetris clone: differently shaped blocks are falling down the
rectangular playing field and can be moved sideways or rotated by 90 degree
units with the aim of building lines without gaps which then disappear (causing
any block above the deleted line to fall down).  LTris has three game modes: In
Classic you play until the stack of blocks reaches the top of the playing field
and no new blocks can enter.  In Figures the playing field is reset to a new
figure each level and later on tiles and lines suddenly appear.  In Multiplayer
up to three players (either human or CPU) compete with each other sending
removed lines to all opponents.  There is also a Demo mode in which you can
watch your CPU playing while enjoying a cup of tea!")
    (license license:gpl2+)))

(define-public nethack
  (package
    (name "nethack")
    (version "3.6.7")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "https://www.nethack.org/download/" version "/nethack-"
                        (string-join (string-split version #\.) "") "-src.tgz"))
        (sha256
          (base32 "1cmc596x8maixi2bkx9kblp3daxw156ahnklc656dygbdpgngkwq"))))
    (native-inputs
      (list bison flex))
    (inputs
      (list ncurses less))
    (build-system gnu-build-system)
    (arguments
      '(#:make-flags
        `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda _
              (substitute* "sys/unix/nethack.sh"
                (("^ *cd .*$") ""))
              (substitute* "sys/unix/Makefile.utl"
                (("^YACC *=.*$") "YACC = bison -y\n")
                (("^LEX *=.*$") "LEX = flex\n")
                (("^# CC = gcc") "CC = gcc"))
              (substitute* "sys/unix/hints/linux"
                (("/bin/gzip") (string-append
                                 (assoc-ref %build-inputs "gzip")
                                 "/bin/gzip"))
                (("^WINTTYLIB=.*") "WINTTYLIB=-lncurses"))
              (substitute* "include/config.h"
                (("^.*define CHDIR.*$") "")
                (("^/\\* *#*define *REPRODUCIBLE_BUILD *\\*/")
                 ;; Honor SOURCE_DATE_EPOCH.
                 "#define REPRODUCIBLE_BUILD"))

              ;; Note: 'makedefs' rejects and ignores dates that are too old
              ;; or too new, so we must choose something reasonable here.
              (setenv "SOURCE_DATE_EPOCH" "1531865062")

              (substitute* "sys/unix/Makefile.src"
                 (("^# CC = gcc") "CC = gcc"))
              #t))
          (replace 'configure
            (lambda _
              (let ((bash (string-append
                            (assoc-ref %build-inputs "bash")
                            "/bin/bash")))
                (with-directory-excursion "sys/unix"
                  (substitute* "setup.sh" (("/bin/sh") bash))
                  (invoke bash "setup.sh" "hints/linux"))
                #t)))
          (add-after 'install 'fixup-paths
            (lambda _
              (let* ((output (assoc-ref %outputs "out"))
                     (nethack-script (string-append output "/bin/nethack")))
                (mkdir-p (string-append output "/games/lib/nethackuserdir"))
                (for-each
                  (lambda (file)
                    (rename-file
                      (string-append output "/games/lib/nethackdir/" file)
                      (string-append output "/games/lib/nethackuserdir/"
                                     file)))
                  '("xlogfile" "logfile" "perm" "record" "save"))
                (mkdir-p (string-append output "/bin"))
                (call-with-output-file nethack-script
                  (lambda (port)
                    (format port "#!~a/bin/sh
PATH=~a:$PATH
if [ ! -d ~~/.config/nethack ]; then
  mkdir -p ~~/.config/nethack
  cp -r ~a/games/lib/nethackuserdir/* ~~/.config/nethack
  chmod -R +w ~~/.config/nethack
fi

RUNDIR=$(mktemp -d)

cleanup() {
  rm -rf $RUNDIR
}
trap cleanup EXIT

cd $RUNDIR
for i in ~~/.config/nethack/*; do
  ln -s $i $(basename $i)
done
for i in ~a/games/lib/nethackdir/*; do
  ln -s $i $(basename $i)
done
~a/games/nethack \"$@\""
                      (assoc-ref %build-inputs "bash")
                      (list->search-path-as-string
                        (list
                          (string-append
                            (assoc-ref %build-inputs "coreutils") "/bin")
                          (string-append
                            (assoc-ref %build-inputs "less") "/bin"))
                        ":")
                      output
                      output
                      output)))
                (chmod nethack-script #o555)
                #t)))
          (delete 'check))))
    (home-page "https://nethack.org")
    (synopsis "Classic dungeon crawl game")
    (description "NetHack is a single player dungeon exploration game that runs
on a wide variety of computer systems, with a variety of graphical and text
interfaces all using the same game engine.  Unlike many other Dungeons &
Dragons-inspired games, the emphasis in NetHack is on discovering the detail of
the dungeon and not simply killing everything in sight - in fact, killing
everything in sight is a good way to die quickly.  Each game presents a
different landscape - the random number generator provides an essentially
unlimited number of variations of the dungeon and its denizens to be discovered
by the player in one of a number of characters: you can pick your race, your
role, and your gender.")
    (license
      (license:fsdg-compatible
        "https://nethack.org/common/license.html"))))

(define-public pipewalker
  (package
    (name "pipewalker")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pipewalker/pipewalker/"
                           version "/pipewalker-" version ".tar.gz"))
       (sha256
        (base32 "1x46wgk0s55562pd96cxagxkn6wpgglq779f9b64ff1k3xzp3myn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-docdir
           ;; Makefile.in ignores configure's ‘--docdir=...’ option.  Fix that.
           (lambda _
             (substitute* "Makefile"
               (("(pkgdocdatadir = ).*" _ assignment)
                (string-append assignment "$(docdir)\n")))
             #t)))))
    (inputs
     (list libpng mesa sdl))
    (home-page "https://pipewalker.sourceforge.net/")
    (synopsis "Logical tile puzzle")
    (description
     "PipeWalker is a simple puzzle game with many different themes: connect all
computers to one network server, bring water from a source to the taps, etc.
The underlying mechanism is always the same: you must turn each tile in the
grid in the right direction to combine all components into a single circuit.
Every puzzle has a complete solution, although there may be more than one.")
    (license license:gpl3+)))

(define-public dsda-doom
  (package
    (name "dsda-doom")
    (version "0.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kraflab/dsda-doom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qvxx4r3ahiy8w9x0559g581971ycmbqm1kszzc65w1aa85f5q2f"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'chdir
                     (lambda _
                       (chdir "prboom2"))))))
    (inputs
     (list sdl2
           sdl2-mixer
           fluidsynth
           portmidi
           libmad
           libzip
           glu
           dumb
           libvorbis))
    (home-page "https://github.com/kraflab/dsda-doom")
    (synopsis "Doom source port, successor of PrBoom+")
    (description
     "This is a successor of PrBoom+ with new features, including:
@enumerate
@item Heretic, Hexen, MBF21, Doom-in-Hexen, UDMF, and MAPINFO support
@item In-game console and scripting
@item Full controller support
@item Palette-based opengl renderer
@item Debugging features for testing
@item Strict mode for speedrunning
@item Various quality of life improvements
@item Advanced tools for TASing
@item Rewind
@end enumerate")
    (license license:gpl2+)))

(define-public prboom-plus
  (package
   (name "prboom-plus")
   (version "2.5.1.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/prboom-plus/prboom-plus/"
                                version "/prboom-plus-" version ".tar.gz"))
            (sha256
             (base32 "151v6nign86m1a2vqz27krsccpc9m4d1jax4y43v2fa82wfj9qp0"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "src/version.c"
                  (("__DATE__") "")
                  (("__TIME__") ""))
                #t))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags '("--disable-cpu-opt" "CFLAGS=-fcommon")
      #:make-flags `(,(string-append "gamesdir="
                                     (assoc-ref %outputs "out") "/bin"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'set-paths 'set-sdl'paths
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "CPATH"
                    (string-append
                     (search-input-directory inputs "/include/SDL")
                     ":" (or (getenv "CPATH") ""))))))))
   (inputs
    (list fluidsynth
          glu
          libmad
          libpng
          libvorbis
          pcre
          portmidi
          (sdl-union (list sdl sdl-image sdl-mixer sdl-net))))
   (home-page "https://prboom-plus.sourceforge.net/")
   (synopsis "Version of the classic 3D shoot'em'up game Doom")
   (description
    "PrBoom+ is a Doom source port developed from the original PrBoom project.")
   (license license:gpl2+)))

(define-public redeal
  (let ((commit "e2e81a477fd31ae548a340b5f0f380594d3d0ad6")
        (revision "1"))
    (package
      (name "redeal")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/anntzer/redeal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1vac36bg4ah9gs4hgmp745xq6nnmd7s71vsq99d72ng3sxap0wa3"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'unbundle-dds
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "setup.py"
                  (("cmdclass=.*") ""))
                (let ((libdds (search-input-file inputs "lib/libdds.so")))
                  (substitute* "redeal/dds.py"
                    ((" and os.path.exists\\(dll_path\\)") "")
                    (("dll = DLL\\(dll_path\\)")
                     (format #f "dll = DLL(~s)" libdds))))))
            (add-after 'install 'install-examples
              (lambda _
                (let* ((doc (string-append #$output "/share/doc/"))
                       (examples
                        (string-append doc #$name "-" #$version "/examples")))
                  (mkdir-p examples)
                  (copy-recursively "examples" examples)))))))
      (inputs (list dds `(,python "tk")))
      (propagated-inputs (list python-colorama))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/anntzer/redeal")
      (synopsis
       "Deal generator for bridge card game, written in Python")
      (description
       "Redeal is a deal generator written in Python.  It outputs deals
satisfying whatever conditions you specify --- deals with a double void, deals
with a strong 2♣ opener opposite a yarborough, etc.  Using Bo Haglund's double
dummy solver, it can even solve the hands it has generated for you.")
      (license license:gpl3))))

(define-public retux
  (let ((release "1.6.2")
        (revision 0))
    (package
      (name "retux")
      (version (if (zero? revision)
                   release
                   (string-append release "-"
                                  (number->string revision))))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/retux-game/retux/"
                                    "releases/download/v"
                                    version "/retux-"
                                    release "-src.zip"))
                (sha256
                 (base32
                  "1fzsjg4k25mxjjc28ykz8n3dx5xzwxnp772fwzz5jy1wrxmjkl4x"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           ;; no setup.py script
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out    (assoc-ref outputs "out"))
                      (bin    (string-append out "/bin"))
                      (data   (string-append out "/share/retux")))
                 (mkdir-p bin)

                 (substitute* "retux.py"
                   ;; Use the correct data directory.
                   (("os\\.path\\.join\\(os\\.path\\.dirname\\(__file__\\), \"data\"\\),")
                    (string-append "\"" data "\",")))

                 (copy-file "retux.py" (string-append bin "/retux"))
                 (copy-recursively "data" data)))))))
      (native-inputs
       (list unzip))
      (inputs
       (list python-sge python-xsge))
      (home-page "https://retux-game.github.io/")
      (synopsis "Action platformer game")
      (description
       "ReTux is an action platformer loosely inspired by the Mario games,
utilizing the art assets from the @code{SuperTux} project.")
      ;; GPL version 3 or later is the license for the code and some art.
      ;; The rest of the licenses are for the art exclusively, as listed in
      ;; data/LICENSES.
      (license (list license:cc0
                     license:cc-by3.0
                     license:cc-by-sa3.0
                     license:cc-by-sa4.0
                     license:gpl2+
                     license:gpl3+)))))

(define-public robotfindskitten
  (package
    (name "robotfindskitten")
    (version "2.8284271.702")            ; 1600003_201b is older, see ChangeLog
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/" name "/" name
                                  "/releases/download/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
                (base32
                 "1bwrkxm83r9ajpkd6x03nqvmdfpf5vz6yfy0c97pq3v3ykj74082"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; there are no tests
      #:make-flags
      #~(list
         ;; Required for colorized output, see <http://bugs.gnu.org/54607>.
         "CFLAGS=-D_XOPEN_SOURCE=600"
         (string-append "execgamesdir=" #$output "/bin"))))
    (inputs (list ncurses))
    (outputs (list "out" "debug"))
    (synopsis "Thematic meditative game")
    (description
     "You are a robot moving around in a realm filled with ASCII characters.
Examine humorously described though useless items as you search for a kitten
among them.  The theme of this Zen simulation is continued in its
documentation.")
    (home-page "http://robotfindskitten.org/")
    (license license:gpl2+)))

(define-public roguebox-adventures
  (package
    (name "roguebox-adventures")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://download.tuxfamily.org/rba/RogueBoxAdventures_v"
             (string-join (string-split version #\.) "_") "_Source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "05zd03s5w9kcpklfgcggbaa6rwf59nm0q9vcj6gh9v2lh402k067"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (invoke "unzip" source)
                  ;; The actual source is buried a few directories deep.
                  (chdir (string-append "RogueBoxAdventures_v"
                                        (string-join
                                         (string-split ,version #\.) "_")
                                        "_Source")))))
         ;; no setup.py script
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (string-append
                           out "/share/games/roguebox-adventures")))
               ;; Use the correct data directory.
               (substitute* '("main.py" "LIB/getch.py" "LIB/getch_gcwz.py")
                 (("basic_path + os\\.sep + 'DATA'")
                  (string-append "'" data "'"))
                 (("^basic_path.*$")
                  (string-append "basic_path ='" data "'\n")))
               (substitute* "LIB/dialog.py"
                 (("d_path = os\\.path\\.dirname\\(.*\\)\\)")
                  (string-append "d_path = '" data "'")))
               (substitute* "LIB/gra_files.py"
                 (("basic_path = b_path\\.replace\\('/LIB',''\\)")
                  (string-append "basic_path ='" data "'\n")))

               ;; The game must save in the user's home directory because
               ;; the store is read-only.
               (substitute* "main.py"
                 (("home_save = False") "home_save = True")
                 (("'icon_small.png'")
                  (string-append "'" data "/icon_small.png'"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (roguebox-adventures
                     (string-append bin "/roguebox-adventures"))
                    (data (string-append
                           out "/share/games/roguebox-adventures"))
                    (lib (string-append data "/LIB"))
                    (doc (string-append
                          out "/share/doc/roguebox-adventures")))
               (mkdir-p bin)
               (mkdir-p doc)

               (for-each (lambda (file)
                           (copy-recursively file
                                             (string-append data "/" file)))
                         '("AUDIO" "FONT" "GRAPHIC" "LIB" "LICENSE"
                           "icon_big.png" "icon_small.png"))
               (for-each (lambda (file)
                           (chmod file #o555)
                           (install-file file lib))
                         '("main.py" "run.py"))

               (copy-recursively "DOC" doc)

               (call-with-output-file
                   roguebox-adventures
                 (lambda (p)
                   (format p "\
#!~a
export GUIX_PYTHONPATH=~a/LIB:~a
exec -a \"~a\" ~a \"$@\"\n"
                           (which "bash") data (getenv "GUIX_PYTHONPATH")
                           (which "python3")
                           (string-append lib "/main.py"))))
               (chmod roguebox-adventures #o555))
             #t)))))
    (native-inputs
     (list unzip))
    (inputs
     (list python-pygame python-tmx))
    (home-page "https://rogueboxadventures.tuxfamily.org")
    (synopsis "Classical roguelike/sandbox game")
    (description
     "RogueBox Adventures is a graphical roguelike with strong influences
from sandbox games like Minecraft or Terraria.  The main idea of RogueBox
Adventures is to offer the player a kind of roguelike toy-world.  This world
can be explored and changed freely.")
    ;; The GPL3+ is for code, the rest are for art.
    (license (list license:cc0
                   license:cc-by3.0
                   license:gpl3+
                   license:silofl1.1))))

(define-public seahorse-adventures
  (package
    (name "seahorse-adventures")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dulsi/seahorse-adventures")
             (commit (string-append "Release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dxysa79cz5mflr2953fyk838h1jwvi1ngn8wlpms0ag35yv21s8"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       ;; Remove non-free (non-commercial) font.
       (snippet
        #~(begin
            (for-each delete-file (find-files "data/fonts" "."))))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)               ;pure Python
          (replace 'install             ;no install script
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (share (string-append #$output "/share"))
                     (applications (string-append share "/applications"))
                     (data (string-append share "/seahorse-adventures")))
                ;; Install data.
                (for-each (lambda (f)
                            (chmod f #o555)
                            (install-file f data))
                          '("leveledit.py" "run_game.py" "tileedit.py"))
                (for-each (lambda (dir)
                            (let ((target (string-append data "/" dir)))
                              (mkdir-p target)
                              (copy-recursively dir target)))
                          '("data" "lib"))
                ;; Create executable.
                (mkdir-p bin)
                (let ((executable (string-append bin "/seahorse-adventures")))
                  (call-with-output-file executable
                    (lambda (p)
                      (format p
                              "#!~a~@
                              export GUIX_PYTHONPATH=~a:~a~@
                              exec -a \"~a\" ~a \"$@\"~%"
                              (search-input-file inputs "/bin/bash")
                              data
                              (getenv "GUIX_PYTHONPATH")
                              (search-input-file inputs "/bin/python3")
                              (string-append data "/run_game.py"))))
                  (chmod executable #o555))
                ;; Add desktop file.
                (mkdir-p applications)
                (make-desktop-entry-file
                 (string-append applications "/seahorse-adventures.desktop")
                 #:name "Seahorse Adventures"
                 #:comment
                 '((#f "Help Barbie the seahorse float on bubbles to the moon"))
                 #:exec #$name
                 #:icon #$name
                 #:categories '("Game" "ActionGame")
                 #:keywords '("game" "retro" "platform"))
                ;; Add icons.
                (for-each
                 (lambda (size)
                   (let ((dir (string-append share "/icons/hicolor/"
                                             size "x" size "/apps")))
                     (mkdir-p dir)
                     (copy-file
                      (string-append "icon" size ".png")
                      (string-append dir "/searhorse-adventures.png"))))
                 '("32" "64" "128")))))
          (add-after 'install 'unbundle-fonts
            ;; Unbundle Bitstream Vera font and replace deleted one.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((data (string-append #$output "/share/seahorse-adventures"))
                    (vera (search-input-file
                           inputs "/share/fonts/truetype/Vera.ttf")))
                (let ((themes-dir (string-append data "/data/themes/")))
                  (for-each
                   (lambda (theme)
                     (let ((target (string-append themes-dir theme "/Vera.ttf")))
                       (delete-file target)
                       (symlink vera target)))
                   '("default" "gray")))
                (symlink vera (string-append data "/data/fonts/04B_20__.TTF"))
                (substitute* (string-append data "/lib/main.py")
                  (("f_scale = 0.35") "f_scale = 0.47"))))))))
    (inputs
     (list font-bitstream-vera python-pygame))
    (home-page "http://www.imitationpickles.org/barbie/")
    (synopsis "Help Barbie the seahorse float on bubbles to the moon")
    (description
     "Barbie Seahorse Adventures is a retro style platform arcade game.
You are Barbie the seahorse who travels through the jungle, up to the
volcano until you float on bubbles to the moon.  On the way to your
final destination you will encounter various enemies, servants of the
evil overlord who has stolen the galaxy crystal.  Avoid getting hit
and defeat them with your bubbles!")
    ;; GPL2+ is for code, CC0 is for art.
    (license (list license:gpl2+ license:cc0))))

(define-public solarus
  (package
    (name "solarus")
    ;; XXX: When updating this package, please also update hash in
    ;; `solarus-quest-editor' below.
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/solarus-games/solarus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ny9dgqphjv2l39rff2621hnrzpf8qin8vmnv7jdz20azjk4m8id"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           ;; The following tests fail reporting a missing "/dev/dri"
           ;; file.
           (lambda _
             (substitute* "tests/cmake/AddTestMaps.cmake"
               ((".*1200_create_shader_from_source.*" all)
                (string-append "#" all))
               ((".*1210_shader_scaling_factor.*" all)
                (string-append "#" all)))
             #t))
         (add-before 'check 'set-home
           ;; Tests fail without setting the following environment
           ;; variables.
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             #t)))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     `(("glm" ,glm)
       ("libmodplug" ,libmodplug)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("luajit" ,luajit)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("qtbase" ,qtbase-5)
       ("sdl2" ,(sdl-union (list sdl2 sdl2-image sdl2-ttf)))))
    (home-page "https://www.solarus-games.org/")
    (synopsis "Lightweight game engine for Action-RPGs")
    (description
     "Solarus is a 2D game engine written in C++, that can run games
scripted in Lua.  It has been designed with 16-bit classic Action-RPGs
in mind.")
    ;; The source code is licensed under the terms of GPL-3.0.
    ;; Resources are licensed under the terms of CC-BY-SA-3.0 and
    ;; CC-BY-SA 4.0.
    (license (list license:gpl3 license:cc-by-sa3.0 license:cc-by-sa4.0))))

(define-public solarus-quest-editor
  (package
    (inherit solarus)
    (name "solarus-quest-editor")
    (version (package-version solarus))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/solarus-games/solarus-quest-editor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pvjgd4faxii5sskw1h55lw90hlbazhwni8nxyywzrmkjbq7irm0"))))
    (arguments
     `(#:tests? #false))                ; no test suite
    (inputs
     (modify-inputs (package-inputs solarus)
       (prepend solarus)))
    (synopsis "Create and modify quests for the Solarus engine")
    (description
     "Solarus Quest Editor is a graphical user interface to create and
modify quests for the Solarus engine.")))

(define-public superstarfighter
  (package
    (name "superstarfighter")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/notapixelstudio/superstarfighter")
             ;; The commit is not tagged upstream:
             ;; https://github.com/notapixelstudio/superstarfighter/commit/350605bf5454c26ebe2c57d8217edd03689c0573
             (commit "32521f467616bb390e3929d07e1936ff43fe64da")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckghzrfgvk9z1n5f4ivnamm6s8h9sbv0a3aq9pp4a3yrhkgld0k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "godot")
             (setenv "HOME" (getcwd))
             (with-output-to-file "export_presets.cfg"
               (lambda ()
                 (display
                  "[preset.0]
name=\"Guix\"
platform=\"Linux/X11\"
runnable=true
[preset.0.options]")))
             #t))
         (replace 'build
           (lambda _
             (let ((godot (assoc-ref %build-inputs "godot-headless")))
               (invoke (string-append godot "/bin/godot_server")
                       "--export-pack" "Guix"
                       "superstarfighter.pck" "project.godot"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (data (string-append share "/superstarfighter"))
                    (icons (string-append share "/icons/hicolor/256x256/apps")))
               (install-file "superstarfighter.pck" data)
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/superstarfighter")
                 (lambda (port)
                   (format port
                           "#!/bin/sh~@
                            exec ~a/bin/godot --main-pack ~a/superstarfighter.pck~%"
                           (assoc-ref inputs "godot")
                           data)
                   (chmod port #o755)))
               (mkdir-p icons)
               (copy-file "icon.png" (string-append icons "/" ,name ".png"))
               (make-desktop-entry-file
                (string-append share "/applications/" ,name ".desktop")
                #:name "SuperStarfighter"
                #:comment "Fast-paced arcade combat game"
                #:exec ,name
                #:icon ,name
                #:categories '("Game" "ArcadeGame")))
             #t)))))
    (native-inputs
     `(("godot-headless" ,godot-lts "headless")))
    (inputs
     (list godot-lts))
    (home-page "https://notapixel.itch.io/superstarfighter")
    (synopsis "Fast-paced local multiplayer arcade game")
    (description "In SuperStarfighter, up to four local players compete in a
2D arena with fast-moving ships and missiles.  Different game types are
available, as well as a single-player mode with AI-controlled ships.")
    (license (list license:expat         ; game
                   license:silofl1.1)))) ; fonts

(define-public tetzle
  (package
    (name "tetzle")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gottcode.org/tetzle/"
                           "tetzle-" version ".tar.bz2"))
       (sha256
        (base32 "0sybryg65j8gz5s7zbsfqky8wlkjwpppkrhksijj6xc7692lfii8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (native-inputs (list qttools))
    (inputs (list qtbase))
    (home-page "https://gottcode.org/tetzle/")
    (synopsis "Jigsaw puzzle game that uses tetrominoes for the pieces")
    (description
     "Tetzle is a jigsaw puzzle game that uses tetrominoes for the pieces.  Any image
can be imported and used to create puzzles with a wide range of sizes.  Games are
saved automatically, and you can select between currently in progress games.")
    (license license:gpl3+)))

(define %ufoai-commit "a542a87a891f96b1ab2c44d35b2f6f16859a5019")
(define %ufoai-revision "0")
(define %ufoai-version (git-version "2.6.0_dev" %ufoai-revision %ufoai-commit))
(define ufoai-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "git://git.code.sf.net/p/ufoai/code") ;HTTPS fails mid-clone
          (commit %ufoai-commit)))
    (file-name (string-append "ufoai-" %ufoai-version "-checkout"))
    (sha256
     (base32
      "024s7b9rcg7iw8i2p72gwnvabk23ljlq0nldws0y4b6hpwzyn1wz"))
    (modules '((guix build utils)
               (srfi srfi-1)
               (ice-9 ftw)))
    (snippet
     '(begin
        ;; Delete ~32MiB of bundled dependencies.
        (with-directory-excursion "src/libs"
          (for-each delete-file-recursively
                    (lset-difference equal? (scandir ".")
                                     '("." ".." "gtest" "mumble"))))

        ;; Use relative path to Lua headers.
        (substitute* "src/common/scripts_lua.h"
          (("\\.\\./libs/lua/") ""))

        ;; Adjust Makefile targets to not depend on 'ufo2map', since we build
        ;; it as a separate package.  This way we don't need to make the same
        ;; adjustments for 'ufoai-data' and 'ufoai' below.
        (substitute* "build/maps.mk"
          (("\\./ufo2map") "ufo2map")
          (("maps: ufo2map") "maps:"))
        (substitute* "build/modules/testall.mk"
          (("testall: ufo2map") "testall:"))

        ;; If no cURL headers are found, the build system will try to include
        ;; the bundled version, even when not required.  Prevent that.
        (substitute* "build/default.mk"
          (("^include src/libs/curl/lib/Makefile\\.inc")
           ""))

        ;; While here, improve reproducibility by adding the '-X' flag to the
        ;; zip command used to create the map files, in order to prevent time
        ;; stamps from making it into the generated archives.
        (substitute* "build/data.mk"
          (("\\$\\(call ZIP\\)")
           "$(call ZIP) -X"))
        #t))))

(define-public trigger-rally
  (package
    (name "trigger-rally")
    (version "0.6.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/trigger-rally/"
                           "trigger-" version "/"
                           "trigger-rally-" version ".tar.gz"))
       (sha256
        (base32
         "016bc2hczqscfmngacim870hjcsmwl8r3aq8x03vpf22s49nw23z"))))
    (build-system gnu-build-system)
    (inputs
     `(("freealut" ,freealut)
       ("glew" ,glew)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image)))
       ("tinyxml2" ,tinyxml2)))
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output)
                          "bindir=$(prefix)/bin"
                          "datadir=$(datarootdir)"
                          "OPTIMS=-Ofast")
       #:tests? #f                      ; No tests present
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'cd-src
           (lambda _ (chdir "src")))
         (add-before 'build 'remove-timestamps
           (lambda _
             (substitute* (list "Trigger/menu.cpp"
                                "PEngine/app.cpp")
               ((".*__DATE__.*") ""))))
         (add-before 'build 'make-verbose
           (lambda _
             (substitute* "GNUmakefile"
               (("@\\$\\(CXX\\)") "$(CXX)"))))
         (add-after 'build 'set-data-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "../bin/trigger-rally.config.defs"
                 (("<data path=\"C:[^\"]*\"")
                  (string-append "<data path=\"" out "/share/trigger-rally\""))))))
         (add-after 'install 'create-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                   (string-append apps "/trigger-rally.desktop")
                 (lambda ()
                   (format #t           ; Borrowed from Debian package
                           "[Desktop Entry]~@
                            Name=Trigger Rally~@
                            Icon=trigger-rally~@
                            Comment=3D rally racing car game~@
                            Comment[de]=3D Rally-Autorennen~@
                            Comment[fr_FR]=un jeu de rally en 3D~@
                            Comment[ro_RO]=Un joc în 3D cu curse de raliu~@
                            Exec=~a/bin/trigger-rally~@
                            Terminal=false~@
                            StartupNotify=false~@
                            Type=Application~@
                            TryExec=~:*~a/bin/trigger-rally~@
                            Categories=Game;ArcadeGame;~@
                            Keywords=racing;tracks;~@
                            Keywords[de]=Rennstrecke;~%"
                           out)))))))))
    (home-page "https://trigger-rally.sourceforge.net")
    (synopsis "Fast-paced single-player racing game")
    (description "Trigger-rally is a 3D rally simulation with great physics
for drifting on over 200 maps.  Different terrain materials like dirt,
asphalt, sand, ice, etc. and various weather, light, and fog conditions give
this rally simulation the edge over many other games.  You need to make it
through the maps in often tight time limits and can further improve by beating
the recorded high scores.  All attached single races must be finished in time
in order to win an event, unlocking additional events and cars.  Most maps are
equipped with spoken co-driver notes and co-driver icons.")
    (license (list license:cc0               ;textures and audio in data.zip
                   license:gpl2+))))

(define-public ufo2map
  (package
    (name "ufo2map")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CC=gcc" "CXX=g++"
                           "--enable-release"
                           "--enable-ufo2map"
                           "--disable-uforadiant"
                           "--disable-cgame-campaign"
                           "--disable-cgame-multiplayer"
                           "--disable-cgame-skirmish"
                           "--disable-game"
                           "--disable-memory"
                           "--disable-testall"
                           "--disable-ufoded"
                           "--disable-ufo"
                           "--disable-ufomodel"
                           "--disable-ufoslicer")
       #:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key (configure-flags '()) #:allow-other-keys)
                      ;; The home-made configure script does not understand
                      ;; some of the default flags of gnu-build-system.
                      (apply invoke "./configure" configure-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "ufo2map" (string-append out "/bin"))
                        (install-file "debian/ufo2map.6"
                                      (string-append out "/share/man/man6"))
                        #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-ttf)))))
    (synopsis "UFO: AI map generator")
    (description
     "This package provides @command{ufo2map}, a program used to generate
maps for the UFO: Alien Invasion strategy game.")
    (license license:gpl2+)))

(define ufoai-data
  (package
    (name "ufoai-data")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags '("CC=gcc" "CXX=g++")
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '()) #:allow-other-keys)
                      (apply invoke "./configure" configure-flags)))
                  (replace 'build
                    (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
                      (invoke "make"
                              "-j" (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1")
                              "maps")))
                  (add-after 'build 'pack
                    (lambda _
                      (invoke "make" "pk3")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each (lambda (file)
                                    (install-file file out))
                                  (find-files "base" "\\.pk3$"))
                        #t))))))
    (native-inputs
     `(("python" ,python-2)
       ("ufo2map" ,ufo2map)
       ("which" ,which)
       ("zip" ,zip)))
    (synopsis "UFO: AI data files")
    (description
     "This package contains maps and other assets for UFO: Alien Invasion.")
    ;; Most assets are available under either GPL2 or GPL2+.  Some use other
    ;; licenses, see LICENSES for details.
    (license (list license:gpl2+ license:gpl2 license:cc-by3.0
                   license:cc-by-sa3.0 license:public-domain))))

(define-public ufoai
  (package
    (name "ufoai")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out"))
             (string-append "--datadir=" (assoc-ref %outputs "out")
                            "/share/games/ufo")
             "CC=gcc" "CXX=g++"
             "--enable-release"
             "--enable-game"
             "--disable-ufo2map"
             "--disable-dependency-tracking"

             ;; Disable hard links to prevent huge NARs.
             "--disable-hardlinkedgame"
             "--disable-hardlinkedcgame")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'symlink-data-files
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((data (assoc-ref inputs "ufoai-data")))
                        ;; Symlink the data files to where the build system
                        ;; expects to find them.  Ultimately these files are
                        ;; copied to $out/share/games/ufoai/base, losing the
                        ;; symlinks; we could fix that after install, but it
                        ;; does not make a big difference in practice due to
                        ;; deduplication.
                        (with-directory-excursion "base"
                          (for-each (lambda (file)
                                      (symlink file (basename file)))
                                    (find-files data "\\.pk3$")))
                        #t)))
                  (add-before 'configure 'create-language-files
                    (lambda _
                      (invoke "make" "lang")))
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '()) #:allow-other-keys)
                      (apply invoke "./configure" configure-flags)))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "./testall")
                          (format #t "test suite not run~%"))
                      #t))
                  (add-after 'install 'install-man-pages
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (man6 (string-append out "/share/man/man6")))
                        (install-file "debian/ufo.6" man6)
                        (install-file "debian/ufoded.6" man6)
                        #t))))

       ;; TODO: Some map tests occasionally fail because of randomness issues,
       ;; e.g. not enough generated aliens.  The test runner also fails early
       ;; in the build container with 'failed to shutdown server'?
       #:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ufo2map" ,ufo2map)
       ("ufoai-data" ,ufoai-data)))
    (inputs
     `(("curl" ,curl)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("lua" ,lua-5.1)
       ("mesa" ,mesa)
       ("minixml" ,minixml)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-ttf)))
       ("zlib" ,zlib)))
    (synopsis "Turn-based tactical strategy game")
    (description
     "UFO: Alien Invasion is a tactical strategy game set in the year 2084.
You control a secret organisation charged with defending Earth from a brutal
alien enemy.  Build up your bases, prepare your team, and dive head-first into
the fast and flowing turn-based combat.

Over the long term you will need to conduct research into the alien threat to
figure out their mysterious goals and use their powerful weapons for your own
ends.  You will produce unique items and use them in combat against your
enemies.

You can also use them against your friends with the multiplayer functionality.

Warning: This is a pre-release version of UFO: AI!  Some things may not work
properly.")

    ;; The game code and most assets are GPL2+, but we use GPL2 only here
    ;; because some assets do not use the "or later" clause.  Many individual
    ;; assets use Creative Commons or Public Domain; see the LICENSE file.
    (license (delete license:gpl2+ (package-license ufoai-data)))))

(define-public xshogi
  (package
    (name "xshogi")
    (version "1.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/xshogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1dns0nhymak44by18sv48m4xb2skiwbi2i3nb9hl6w9iwd2i2brf"))))
    (build-system gnu-build-system)
    (inputs
     (list libxaw libxt))
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "User interface for gnushogi")
    (description  "A graphical user interface for the package @code{gnushogi}.")
    ;; Contains a copy of GPLv3 but the licence notices simply
    ;; state "GNU General Public Licence" without specifying a version.
    (license license:gpl1+)))

(define-public abbaye
  (package
    (name "abbaye")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nevat/abbayedesmorts-gpl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16zxmz7z6jfawh68q8k9s1iwbl2f9jr3qaiqlkwpz8vmpqw2s47x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list "CC=gcc")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'set-sdl-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CPATH"
                      (string-append
                       (search-input-directory inputs "include/SDL2")
                       ":" (or (getenv "CPATH") "")))))
          (add-after 'patch-source-shebangs 'patch-Makefile-prefix
            (lambda _
              (substitute* "Makefile"
                (("/usr") #$output))))
          (add-before 'install 'create-directories
            (lambda _
              ;; Create directories that the makefile assumes exist.
              (mkdir-p (string-append #$output "/bin"))
              (mkdir-p (string-append #$output "/share/applications"))
              (mkdir-p (string-append #$output "/share/pixmaps"))))
          (delete 'configure))          ;no configure script
      #:tests? #f)) ;no test suite
    (native-inputs (list pkg-config))
    (inputs (list (sdl-union (list sdl2 sdl2-image sdl2-mixer))))
    (home-page "https://github.com/nevat/abbayedesmorts-gpl")
    (synopsis "GNU/Linux port of the indie game \"l'Abbaye des Morts\"")
    (description "L'Abbaye des Morts is a 2D platform game set in 13th century
France.  The Cathars, who preach about good Christian beliefs, were being
expelled by the Catholic Church out of the Languedoc region in France.  One of
them, called Jean Raymond, found an old church in which to hide, not knowing
that beneath its ruins lay buried an ancient evil.")
    (license license:gpl3)))

(define-public angband
  (package
    (name "angband")
    (version "4.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/angband/angband")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kg6npbfy42mhggsqvs04khc8198i980z52xm59pws29698qazaw"))
       (modules '((guix build utils)))
       (snippet
        ;; So, some of the sounds/graphics/tilesets are under different
        ;; licenses... some of them even nonfree!  This is a console-only
        ;; version of this package so we just remove them.
        ;; In the future, if someone tries to make a graphical variant of
        ;; this package, they can deal with that mess themselves. :)
        '(begin
           (for-each (lambda (subdir)
                       (let ((lib-subdir (string-append "lib/" subdir)))
                         (delete-file-recursively lib-subdir)))
                     '("fonts" "icons" "sounds" "tiles"))
           (substitute* "lib/Makefile"
             ;; And don't try to invoke makefiles in the directories we removed.
             (("gamedata customize help screens fonts tiles sounds icons user")
              "gamedata customize help screens user"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags (list (string-append "--bindir=" %output "/bin"))))
    (native-inputs (list autoconf automake))
    (inputs (list ncurses))
    (home-page "https://rephial.org/")
    (synopsis "Dungeon exploration roguelike")
    (description "Angband is a Classic dungeon exploration roguelike.  Explore
the depths below Angband, seeking riches, fighting monsters, and preparing to
fight Morgoth, the Lord of Darkness.")
    (license license:gpl2)))

(define-public pingus
  (package
    (name "pingus")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/pingus/pingus.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wp06kcmknsnxz7bjnsndb8x062z7r23fb3yrnbfnj68qhz18y74"))
       (patches (search-patches "pingus-boost-headers.patch"
                                "pingus-sdl-libs-config.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "src/pingus/screens/demo_session.cpp"
             (("#include <iostream>")
              ;; std::function moved to <functional> with C++ 11.
              ;; Remove this for versions newer than 0.7.6.
              "#include <iostream>\n#include <functional>"))
           #t))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config scons-python2))
    (inputs (list sdl
                  sdl-image
                  sdl-mixer
                  mesa
                  glu
                  libpng
                  boost))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (home-page "https://pingus.seul.org/")
    (synopsis "Lemmings clone")
    (description
     "Pingus is a free Lemmings-like puzzle game in which the player takes
command of a bunch of small animals and has to guide them through levels.
Since the animals walk on their own, the player can only influence them by
giving them commands, like build a bridge, dig a hole, or redirect all animals
in the other direction.  Multiple such commands are necessary to reach the
level's exit.  The game is presented in a 2D side view.")
    ;; Some source files are under bsd-3 and gpl2+ licenses.
    (license license:gpl3+)))

(define-public talkfilters
  (package
    (name "talkfilters")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hyperrealm.com/talkfilters/"
                           "talkfilters-" version  ".tar.gz"))
       (sha256
        (base32 "19nc5vq4bnkjvhk8srqddzhcs93jyvpm9r6lzjzwc1mgf08yg0a6"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/talkfilters/")
    (synopsis "Convert English text to humorous dialects")
    (description "The GNU Talk Filters are programs that convert English text
into stereotyped or otherwise humorous dialects.  The filters are provided as
a C library, so they can easily be integrated into other programs.")
    (license license:gpl2+)))

(define-public taisei
  (package
    (name "taisei")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/taisei-project/"
                           "taisei/releases/download/v" version
                           "/taisei-" version ".tar.xz"))
       (sha256
        (base32 "19sgm175clkvpcv0b9p4jkjpfxqw0kyl2i5p8w63kwzqcjp9m1jx"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release" ;comment out for bug-reporting (and cheats)
      #:configure-flags #~(list "-Dr_default=gles30"
                                "-Dr_gles30=enabled"
                                "-Dshader_transpiler=enabled")))
    (native-inputs
     (list pkg-config
           python
           python-docutils
           python-pygments
           python-zstandard))
    (inputs
     (list cglm
           freetype
           libpng
           libwebp
           libzip
           mesa
           openssl
           opusfile
           sdl2
           sdl2-mixer
           shaderc
           spirv-cross
           zlib
           (list zstd "lib")))
    (home-page "https://taisei-project.org/")
    (synopsis "Shoot'em up fangame and libre clone of Touhou Project")
    (description
     "The player controls a character (one of three: Good, Bad, and Dead),
dodges the missiles (lots of it cover the screen, but the character's hitbox
is very small), and shoot at the adversaries that keep appear on the screen.")
    (license (list
              ;; game
              license:expat
              ;; resources/00-taisei.pkgdir/bgm/
              ;; atlas/portraits/
              license:cc-by4.0
              ;; miscellaneous
              license:cc0
              license:public-domain))))

(define-public cmatrix
  (package
    (name "cmatrix")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abishekvashok/cmatrix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1h9jz4m4s5l8c3figaq46ja0km1gimrkfxm4dg7mf4s84icmasbm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This old ‘configure’ script doesn't support
             ;; variables passed as arguments.
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (inputs (list ncurses))
    (home-page "https://www.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license license:gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1gg9764ld7skn7jps9pma6x8zqf9nka1cf5nryq197f6lpp404fq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-shell-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chdir bin)
               (substitute* '("gnuchessx" "gnuchessu")
                 (("^gnuchess") (string-append bin "/gnuchess")))))))))
    (home-page "https://www.gnu.org/software/chess/")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (properties '((upstream-name . "gnuchess")
                  (ftp-directory . "/chess")))
    (license license:gpl3+)))

(define-public freedink-engine
  (package
    (name "freedink-engine")
    (version "109.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (patches (search-patches "freedink-engine-fix-sdl-hints.patch"))
              (sha256
               (base32
                "00hhk1bjdrc1np2qz44sa5n1mb62qzwxbvsnws3vpms6iyn3a2sy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-embedded-resources")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-graphical-tests
           (lambda _
             ;; These tests require a graphical interface.
             (substitute* "src/Makefile.am"
               (("test_gfx_fonts TestIOGfxDisplay") ""))
             #t))
         (add-before 'bootstrap 'autoreconf
           (lambda _
	     ;; automake is out of date in the source
	     ;; autoreconf updates the automake scripts
	     (invoke "autoreconf")
	     ;; Build fails when autom4te.cache exists.
	     (delete-file-recursively "autom4te.cache")
             #t))
         (add-after 'install 'delete-freedinkedit-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
             ;; freedinkedit does not know where to find freedink data
             ;; freedink data is read-only, so it cannot be edited anyway.
             ;; TODO: fix freedink.desktop
             (delete-file-recursively (string-append
                            out "/share/applications"))
             #t))))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("cxxtest" ,cxxtest)
                     ("gettext" ,gettext-minimal)
                     ("help2man" ,help2man)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer
                                             sdl2-ttf sdl2-gfx)))
              ("fontconfig" ,fontconfig)
              ("glm" ,glm)))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Twisted adventures of young pig farmer Dink Smallwood")
    (description
     "GNU FreeDink is a free and portable re-implementation of the engine
for the role-playing game Dink Smallwood.  It supports not only the original
game data files but it also supports user-produced game mods or \"D-Mods\".
To that extent, it also includes a front-end for managing all of your D-Mods.")
    (license license:gpl3+)))

(define-public freedink-data
  (package
    (name "freedink-data")
    (version "1.08.20190120")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17gvryadlxk172mblbsil7hina1z5wahwaxnr6g3mdq57dvl8pvi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check))               ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (properties '((ftp-directory . "/freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

(define-public freedink-dfarc
  (package
    (name "freedink-dfarc")
    (version "3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/dfarc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yp8n3w426xnlp10xk06vfi2y3k9xrcfyck7s7qs1v0ys7n284d5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool))
    (inputs
     (list bzip2 wxwidgets))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "dfarc")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Front-end for managing and playing Dink Modules")
    (description "DFArc makes it easy to play and manage the GNU FreeDink game
and its numerous D-Mods.")
    (license license:gpl3+)))

(define-public freedink
  ;; This is a wrapper that tells the engine where to find the data.
  (package (inherit freedink-engine)
    (name "freedink")
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((output     (assoc-ref %outputs "out"))
                          (bin        (string-append output "/bin"))
                          (executable (string-append bin "/freedink")))
                     (mkdir-p bin)
                     (call-with-output-file executable
                       (lambda (port)
                         (format port "#!~a/bin/sh
exec ~a/bin/freedink -refdir ~a/share/dink\n"
                                 (assoc-ref %build-inputs "bash")
                                 (assoc-ref %build-inputs "engine")
                                 (assoc-ref %build-inputs "data"))
                         (chmod port #o777)))
                     #t))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

(define-public fuzzylite
  (package
    (name "fuzzylite")
    (version "6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fuzzylite/fuzzylite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yay0qc81x0irlvxqpy7jywjxpkmpjabdhq2hdh28r9z85wp2nwb"))
              (patches (search-patches "fuzzylite-use-catch2.patch"
                                       "fuzzylite-soften-float-equality.patch"
                                       "fuzzylite-relative-path-in-tests.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'switch-to-fuzzylite-dir
                    (lambda _
                      (chdir "fuzzylite"))))))
    (native-inputs (list catch2))
    (home-page "https://www.fuzzylite.com/")
    (synopsis "Fuzzy logic control binary")
    (description
     "This package provides fuzzylite, a fuzzy logic control library which
allows one to easily create fuzzy logic controllers in a few steps utilizing
object-oriented programming.")
    (license license:gpl3)))

(define-public xboard
  (package
    (name "xboard")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1mkh36xnnacnz9r00b5f9ld9309k32jv6mcavklbdnca8bl56bib"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         ;; Fixes https://issues.guix.gnu.org/47195.
         (add-after 'unpack 'patch-aplay-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xboard.conf"
               (("aplay -q")
                (string-append (search-input-file inputs "/bin/aplay")
                               " -q")))))
         ;; Fixes https://issues.guix.gnu.org/45236.
         (add-after 'unpack 'patch-default-engine
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xboard.conf"
               (("-firstChessProgram fairymax")
                (string-append "-firstChessProgram "
                               (assoc-ref inputs "chess")
                               "/bin/gnuchessx"))))))))
    (inputs
     (list alsa-utils chess gtk+-2 (librsvg-for-system)))
    (native-inputs
     (list texinfo pkg-config))
    (home-page "https://www.gnu.org/software/xboard/")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license license:gpl3+)))

(define-public gtypist
  (package
    (name "gtypist")
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gtypist/gtypist-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04rlh706p5drdvm1kvrgs3jaz9krxg4vi5fpipi4973vb42ymz89"))
              (modules '((guix build utils)))
              (snippet
               ;; We do not provide `ncurses.h' within an `ncursesw'
               ;; sub-directory, so patch the source accordingly.  See
               ;; <http://bugs.gnu.org/19018>.
               '(begin
                  (for-each (lambda (file)
                              (substitute* file
                                (("ncursesw/ncurses.h")
                                 "ncurses.h")))
                            (find-files "." "configure$|\\.c$"))))))
    (build-system gnu-build-system)
    (native-inputs (list emacs-minimal))
    (inputs (list ncurses perl))
    (home-page "https://www.gnu.org/software/gtypist/")
    (synopsis "Typing tutor")
    (description
     "GNU Typist is a universal typing tutor.  It can be used to learn and
practice touch-typing.  Several tutorials are included; in addition to
tutorials for the standard QWERTY layout, there are also tutorials for the
alternative layouts Dvorak and Colemak, as well as for the numpad.  Tutorials
are primarily in English, however some in other languages are provided.")
    (license license:gpl3+)))

(define-public irrlicht
  (package
    (name "irrlicht")
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0gagjh2l3a3m8hsixxhhhan3m5xl7735ka8m4g79jl4qsgp7pyzg"))
              (patches (search-patches "irrlicht-use-system-libs.patch"
                                       "irrlicht-link-against-needed-libs.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                     '("bin" ; bundled compiled Windows binaries"
                       "source/Irrlicht/MacOSX"
                       "source/Irrlicht/bzip2"
                       "source/Irrlicht/jpeglib"
                       "source/Irrlicht/libpng"
                       "source/Irrlicht/lzma"
                       "source/Irrlicht/zlib"))
                  (delete-file "source/Irrlicht/glext.h")
                  (delete-file "source/Irrlicht/glxext.h")
                  (delete-file "source/Irrlicht/wglext.h")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             ;; The actual source is buried a few directories deep.
             (chdir "source/Irrlicht/")))
         (add-after 'chdir-to-source 'remove-<sys/sysctl.h>
           (lambda _
             (substitute* "COSOperator.cpp"
               (("#include <sys/sysctl.h>") ""))))
         (add-after 'chdir-to-source 'delete-broken-install-rule
           (lambda _
             (substitute* "Makefile"
               ;; We neither build nor want a static library.  Skip it.
               ((".*\\bcp .*\\$\\(STATIC_LIB\\).*") ""))))
         (add-after 'chdir-to-source 'fix-build-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("INSTALL_DIR = /usr/local/lib")
                  (string-append "INSTALL_DIR = " out "/lib"))
                 ;; Add '-fpermissive' to the CXXFLAGS.
                 (("-Wall") "-Wall -fpermissive")) ; CImageLoaderJPG.cpp
               ;; The Makefile assumes these directories exist.
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/include")))))
         (delete 'configure))           ; no configure script
       #:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "sharedlib")))
    (inputs
     (list bzip2
           libjpeg-turbo
           libpng
           libx11
           libxxf86vm
           mesa))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "https://irrlicht.sourceforge.io/")
    (license license:zlib)))

(define-public mars
  ;; The latest release on SourceForge relies on an unreleased version of SFML
  ;; with a different API, so we take the latest version from the official
  ;; repository on Github.
  (let ((commit   "84664cda094efe6e49d9b1550e4f4f98c33eefa2")
        (revision "2"))
    (package
      (name "mars")
      (version (git-version "0.7.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelaui/M.A.R.S.")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0bdi4ja39rark742qvqixm8khai5k8qd84z5kzim9jcjdvvwyqj9"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f        ; There are no tests
         #:configure-flags (list (string-append "-Dmars_EXE_DEST_DIR="
                                                %output "/bin"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-data-path
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/System/settings.cpp"
                (("C_dataPath = \"./data/\";")
                 (string-append "C_dataPath = \""
                                (assoc-ref outputs "out")
                                "/share/games/marsshooter/\";"))))))))
      (inputs
       (list mesa fribidi taglib sfml))
      (home-page "https://mars-game.sourceforge.net/")
      (synopsis "2D space shooter")
      (description
       "M.A.R.S. is a 2D space shooter with pretty visual effects and
attractive physics.  Players can battle each other or computer controlled
enemies in different game modes such as space ball, death match, team death
match, cannon keep, and grave-itation pit.")
      (license license:gpl3+))))

(define-public alienblaster
  (package
    (name "alienblaster")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.schwardtnet.de/alienblaster/archives/"
                           "alienblaster-" version ".tgz"))
       (sha256
        (base32
         "104rfsfsv446n4y52p5zw9h8mhgjyrbca8fpyhnxkkasq141a264"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-sdl-paths
            (lambda _
              (let ((share (string-append #$output "/share")))
                ;; Fix name and append path to "SDL_mixer.h"
                (substitute* "src/Makefile"
                  (("GAME_NAME=alienBlaster") "GAME_NAME=alienblaster")
                  (("SDL_FLAGS=\\$\\(shell sdl-config --cflags\\)" line)
                   (string-append line " -I"
                                  #$(this-package-input "sdl-mixer")
                                  "/include/SDL")))
                ;; Substitute relative paths in ".cfg" and source/header files
                (substitute* (find-files "./cfg")
                  (("(\\./)?images") (string-append share "/images")))
                (substitute* (list "src/global.h" "src/global.cc")
                  (("./images") (string-append share "/images"))
                  (("./sound") (string-append share "/sound"))
                  (("./cfg") (string-append share "/cfg"))))))
          (delete 'configure)
          (replace 'install
            (lambda _
              (install-file "alienblaster" (string-append #$output "/bin"))
              (for-each
               (lambda (dir)
                 (copy-recursively dir (string-append #$output "/share/" dir)))
               '("images" "sound" "cfg")))))))
    (inputs (list sdl sdl-mixer))
    (home-page "http://www.schwardtnet.de/alienblaster/")
    (synopsis "Action-loaded 2D arcade shooter game")
    (description "Alien Blaster is an action-loaded 2D arcade shooter
game.  Your mission in the game is simple: stop the invasion of the aliens by
blasting them.  Simultaneous two-player mode is available.")
    (license license:gpl2)))

(define glkterm
  (package
   (name "glkterm")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glk/implementations/glkterm-104.tar.gz"))
     (sha256
      (base32
       "0zlj9nlnkdlvgbiliczinirqygiq8ikg5hzh5vgcmnpg9pvnwga7"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses))) ; required by Make.glkterm
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (inc (string-append out "/include"))
                   (lib (string-append out "/lib")))
              (for-each
               (lambda (file)
                 (install-file file inc))
               '("glk.h" "glkstart.h" "gi_blorb.h" "gi_dispa.h" "Make.glkterm"))
              (install-file "libglkterm.a" lib))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "https://www.eblong.com/zarf/glk/")
   (synopsis "Curses Implementation of the Glk API")
   (description
    "Glk defines a portable API for applications with text UIs.  It was
primarily designed for interactive fiction, but it should be suitable for many
interactive text utilities, particularly those based on a command line.
This is an implementation of the Glk library which runs in a terminal window,
using the @code{curses.h} library for screen control.")
   (license (license:fsf-free "file://README"))))

(define-public glulxe
  (package
   (name "glulxe")
   (version "0.5.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glulx/interpreters/glulxe/glulxe-054.tar.gz"))
     (sha256
      (base32
       "0vipydg6ra90yf9b3ipgppwxyb2xdhcxwvirgjy0v20wlf56zhhz"))))
   (build-system gnu-build-system)
   (inputs `(("glk" ,glkterm)))
   (arguments
    '(#:tests? #f                       ; no check target
      #:make-flags
      (let* ((glk (assoc-ref %build-inputs "glk")))
        (list (string-append "GLKINCLUDEDIR=" glk "/include")
              (string-append "GLKLIBDIR=" glk "/lib")
              (string-append "GLKMAKEFILE=" "Make.glkterm")))
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (install-file "glulxe" bin))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "https://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using the Glk API.")
   (license license:expat)))

(define-public fifechan
  (package
    (name "fifechan")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/fifengine/"
                                  "fifechan/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wxs9vz5x9y8chghd8vp7vfk089lfb0qnzggi17zrqkrngs5zgi9"))))
    (build-system cmake-build-system)
    (inputs
     (list sdl2 sdl2-image mesa))
    (arguments
     '(#:tests? #f))                    ; No included tests
    (home-page "https://fifengine.github.io/fifechan/")
    (synopsis "Cross platform GUI library specifically for games")
    (description
     "Fifechan is a lightweight cross platform GUI library written in C++
specifically designed for games.  It has a built in set of extendable GUI
Widgets, and allows users to create more.")
    (license license:lgpl2.1+)))

(define-public fifengine
  (package
    (name "fifengine")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/fifengine/"
                                  "fifengine/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "fifengine-swig-compat.patch"
                                       "fifengine-boost-compat.patch"
                                       "fifengine-python-3.9-compat.patch"))
              (sha256
               (base32
                "1y4grw25cq5iqlg05rnbyxw1njl11ypidnlsm3qy4sm3xxdvb0p8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f         ;TODO The test running fails to run some tests.
           #:configure-flags
           #~(list
              (string-append "-DOPENALSOFT_INCLUDE_DIR="
                             (search-input-directory %build-inputs "include/AL"))
              (string-append "-DPYTHON_SITE_PACKAGES="
                             #$output "/lib/python"
                             #$(version-major+minor
                                (package-version (this-package-input "python")))
                             "/site-packages"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-run_tests.py
                 (lambda _
                   ;; Patch the test runner to exit with a status of 1 if any test
                   ;; fails, to allow detecting failures.
                   (substitute* "run_tests.py"
                     (("ERROR\\. One or more tests failed!'\\)")
                      "ERROR. One or more tests failed!')
\t\texit(1)"))))
               ;; Run tests after installation so that we can make use of the built
               ;; python modules.
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; The tests require an X server.
                     (system "Xvfb :1 &")
                     (setenv "DISPLAY" ":1")
                     (setenv "XDG_RUNTIME_DIR" "/tmp")
                     ;; Run tests
                     (chdir #$(string-append "../" (package-name this-package)
                                             "-" (package-version this-package)))
                     (invoke "python3" "run_tests.py" "-a")))))))
    (native-inputs
     (list python swig xorg-server-for-tests))
    (inputs
     (list sdl2
           sdl2-image
           sdl2-ttf
           tinyxml
           openal
           libogg
           glew
           libvorbis
           boost
           fifechan
           swig
           python))
    (propagated-inputs
     (list python-future))
    (home-page "https://www.fifengine.net/")
    (synopsis "FIFE is a multi-platform isometric game engine written in C++")
    (description
     "@acronym{FIFE, Flexible Isometric Free Engine} is a multi-platform
isometric game engine.  Python bindings are included allowing users to create
games using Python as well as C++.")
    (license license:lgpl2.1+)))

(define-public fizmo
  (package
    (name "fizmo")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fizmo.spellbreaker.org/source/"
                                  "fizmo-" version ".tar.gz"))
              (sha256
               (base32
                "1amyc4n41jf08kxmdgkk30bzzx54miaxa97w28f417qwn8lrl98w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (let ((libjpeg (assoc-ref %build-inputs "libjpeg"))
             (ncurses (assoc-ref %build-inputs "ncurses")))
         (list (string-append "--with-jpeg-includedir=" libjpeg "/include")))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("freetype" ,freetype)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("sdl2" ,sdl2)))
    (home-page "https://fizmo.spellbreaker.org/")
    (synopsis "Z-machine interpreter")
    (description
     "Fizmo is a console-based Z-machine interpreter.  It is used to play
interactive fiction, also known as text adventures, which were implemented
either by Infocom or created using the Inform compiler.")
    (license license:bsd-3)))

(define-public gnugo
  (package
    (name "gnugo")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gnugo/gnugo-" version
                                 ".tar.gz"))
             (sha256
              (base32
               "0wkahvqpzq6lzl5r49a4sd4p52frdmphnqsfdv7gdp24bykdfs6s"))))
    (build-system gnu-build-system)
    (inputs
     (list readline))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (synopsis "Play the game of Go")
    (description
     "GNU Go is a program that plays the game of Go, in which players
place stones on a grid to form territory or capture other stones.  While
it can be played directly from the terminal, rendered in ASCII characters,
it is also possible to play GNU Go with 3rd party graphical interfaces or
even in Emacs.  It supports the standard game storage format (SGF, Smart
Game Format) and inter-process communication format (GMP, Go Modem
Protocol).")
    (home-page "https://www.gnu.org/software/gnugo/")
    (license license:gpl3+)))

(define-public extremetuxracer
  (package
    (name "extremetuxracer")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "0knd22lzhzqih1w92y6m7yxha376c6ydl22wy4xm6jg2x5jlk1qw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glu sfml))
    (synopsis "High-speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian.
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "https://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))

(define-public exult
  (package
    (name "exult")
    (version "1.8")
    (source
     (origin
       ;; The release tarball isn't bootstrapped, and Git is more robust (SWH).
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exult/exult")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qfbkz05w8989vafc6dvw1wmdi1mvkr4kkgk3ccixadf4616kcb3"))))
    (build-system gnu-build-system)
    (outputs (list "out" "gimp" "studio"))
    (arguments
     (list #:configure-flags
           #~(list "--enable-shared"
                   "--disable-static"
                   "--enable-lto"
                   "--enable-exult-studio"
                   "--enable-exult-studio-support"
                   "--enable-compiler"
                   "--enable-mods"      ; needs --enable-compiler!
                   "--enable-gimp-plugin"
                   ;; A few lines on stdout can save a lot of head-scratching:
                   "CPPFLAGS=-DDEBUG_PATHS=1")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'patch-game-home-directory
                 (lambda _
                   (substitute* "gamemgr/modmgr.cc"
                     ;; EXULT_DATADIR is in the store where it's rather hard for
                     ;; users to put game assets.  Use a more writable home by
                     ;; default, which users can override in their ~/.exult.cfg.
                     (("<GAMEHOME>")
                      (string-append "<HOME>/.local/share/exult"))
                     ;; …however, this causes a regression: the mods which we'll
                     ;; install to EXULT_DATADIR are no longer found.  So: don't
                     ;; look for mods alongside the assets by default.  This too
                     ;; can be overridden in users' ~/.exult.cfg.
                     (("game_path( \\+ \"/mods\")" _ +suffix)
                      (string-append "get_system_path(\"<GAMEHOME>/\") + "
                                     "cfgname" +suffix)))))
               (add-before 'bootstrap 'move-exult-studio
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "studio")))
                     (substitute* "mapedit/studio.cc"
                       (("(esdir, )EXULT_DATADIR" _ prefix)
                        (string-append prefix "\"" out "/share/exult\"")))
                     (substitute* "data/Makefile.am"
                       (("(estudionewdir =.*)\\$\\(datadir\\)(.*)"
                         _ variable= suffix)
                        (string-append variable= out "/share" suffix "\n"))))))
               (add-before 'bootstrap 'fix-gimp-plug-in-prefix
                 ;; ./configure will propagate this value to myriad Makefiles
                 ;; scattered across the tree, so fix it early.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "gimp")))
                     (substitute* "configure.ac"
                       (("(GIMP_PLUGIN_PREFIX=).*" _ variable=)
                        (string-append variable= out "/lib/gimp/2.0"))))))
               (add-after 'install 'move-exult_studio
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((source (assoc-ref outputs "out"))
                         (target (assoc-ref outputs "studio"))
                         (file   "/bin/exult_studio"))
                     (mkdir-p (string-append target (dirname file)))
                     (rename-file (string-append source file)
                                  (string-append target file))))))))
    (native-inputs
     (list autoconf automake libtool pkg-config
           ;; The following are needed only by the GIMP plug-in.
           gimp libjpeg-turbo
           gegl gtk+-2                  ; needed by gimpui-2.0.pc
           ;; The following are needed only by the Usecode compiler.
           bison flex))
    (inputs
     (list fluidsynth freetype libvorbis sdl2
           ;; GTK is needed only by Exult Studio.
           gtk+))
    (synopsis "Role-playing game engine compatible with Ultima VII")
    (description
     "Exult is an Ultima 7 game engine that runs on modern operating systems.
Ultima 7 (or Ultima VII) is a two-part @acronym{RPG, role-playing game} from the
early 1990s.

Exult is fully compatible with the original Ultima 7, but doesn't require any
of its data files to be useful.  Explore entirely new game worlds---or create
your own with the included game and map editor, Exult Studio.

This package expects the game(s) to be placed in subdirectories of
@file{~/.local/share/exult}.")
    (home-page "http://exult.info/")
    (license license:gpl2+)))

(define %supertuxkart-version "1.4")
(define supertuxkart-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/supertuxkart/stk-code")
          (commit %supertuxkart-version)))
       (sha256
        (base32
         "1hv4p0430zw6qm5fgsmayhj8hdxx7qpzggwks5w26z0dz1b5m9w2"))
       (file-name (git-file-name "supertuxkart" %supertuxkart-version))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled library sources
        '(begin
           ;; Supertuxkart uses modified versions of the Irrlicht engine
           ;; and the bullet library.  The developers gave an explanation
           ;; here: http://forum.freegamedev.net/viewtopic.php?f=17&t=3906
           ;; FIXME: try to unbundle angelscript and libraqm
           (for-each delete-file-recursively
                     '("lib/dnsc"
                       "lib/enet"
                       "lib/mcpp"
                       "lib/mojoal"
                       "lib/wiiuse"))))))

(define supertuxkart-data
  ;; There are no tags or releases for the stk-assets data, nor indication of
  ;; which revision is bundled into the released SuperTuxKart-*-src tarball;
  ;; use the latest SVN revision available.
  (let ((commit "18593")
        (revision "0"))
    (hidden-package
     (package
       (name "supertuxkart-data")
       ;; The package produced is a merger of supertuxkart's "stk-assets"
       ;; repository and the "stk-code" repository's "data" directory, so
       ;; include the code version as well.
       (version (string-append %supertuxkart-version "-" commit))
       (source
        (origin
          (method svn-fetch)
          (uri (svn-reference
                (url "https://svn.code.sf.net/p/supertuxkart/code/stk-assets")
                (revision (string->number commit))))
          (file-name (string-append name "-" commit "-checkout"))
          (sha256
           (base32
            "0x2l45w1ahgkw9mrbcxzwdlqs7rams6rsga9m40qjapfiqmvlvbg"))))
       (build-system copy-build-system)
       (arguments
        (list #:install-plan
              #~'(("." "share/supertuxkart/data"
                   #:exclude-regexp ("wip-.*")))
              #:phases
              #~(modify-phases %standard-phases
                  (add-after 'unpack 'copy-code-data
                    (lambda _
                      (copy-recursively
                       (string-append
                        #$(this-package-input
                           (git-file-name "supertuxkart" %supertuxkart-version))
                        "/data/")
                       "."))))))
       (inputs (list supertuxkart-source))
       (home-page "https://supertuxkart.net/Main_Page")
       (synopsis "Data files for SuperTuxKart")
       (description "This package contains data files for SuperTuxKart.")
       (license (list license:gpl3+
                      license:cc-by-sa3.0
                      license:cc-by-sa4.0
                      license:cc0))))))

(define-public supertuxkart
  (package
    (name "supertuxkart")
    (version %supertuxkart-version)
    (source supertuxkart-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ; no check target
           #:configure-flags
           #~(list "-DCHECK_ASSETS=FALSE" ; assets are out-of-tree
                   (string-append "-DSTK_INSTALL_DATA_DIR_ABSOLUTE="
                                  #$(this-package-input "supertuxkart-data")
                                  "/share/supertuxkart")
                   "-DUSE_WIIUSE=0"
                   "-DUSE_SYSTEM_ENET=TRUE"
                   "-DUSE_CRYPTO_OPENSSL=TRUE"
                   ;; In order to use the system ENet library, IPv6 support
                   ;; (added in SuperTuxKart version 1.1) must be disabled.
                   "-DUSE_IPV6=FALSE")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'disable-data-install
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("^install\\(DIRECTORY.*STK_DATA_DIR" &)
                      (string-append "# " &))))))))
    (inputs
     (list curl
           freetype
           fribidi
           glew
           harfbuzz
           libopenglrecorder
           libvorbis
           libx11
           libxrandr
           mesa
           openal
           sdl2
           sqlite
           supertuxkart-data
           zlib
           ;; The following input is needed to build the bundled and modified
           ;; version of irrlicht.
           enet
           libjpeg-turbo
           openssl))
    (native-inputs (list mcpp pkg-config python))
    (home-page "https://supertuxkart.net/Main_Page")
    (synopsis "3D kart racing game")
    (description "SuperTuxKart is a 3D kart racing game, with a focus on
having fun over realism.  You can play with up to 4 friends on one PC, racing
against each other or just trying to beat the computer; single-player mode is
also available.")
    (license license:gpl3+)))

(define-public unknown-horizons
  (package
    (name "unknown-horizons")
    (version "2019.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/unknown-horizons/"
                                  "unknown-horizons/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n747p7h0qp48szgp262swg0xh8kxy1bw8ag1qczs4i26hyzs5x4"))
              (patches (search-patches "unknown-horizons-python-3.8-distro.patch"
                                       "unknown-horizons-python-3.9.patch"
                                       "unknown-horizons-python-3.10.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-after 'build 'build-extra
           (lambda _
             (invoke "python3" "./setup.py" "build_i18n")
             (invoke "python3" "horizons/engine/generate_atlases.py" "2048")
             #t))
         (add-after 'install 'patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/bin/unknown-horizons")
                 (("os\\.chdir\\(get\\_content\\_dir\\_parent_path\\(\\)\\)")
                  (string-append "os.chdir(\""
                                 (assoc-ref outputs "out")
                                 "/share/unknown-horizons\")"))))
             #t))
         (add-before 'check 'fix-tests-with-pytest>=4
           (lambda _
             (substitute* "tests/conftest.py"
               (("pytest_namespace")
                "pytest_configure")
               (("get_marker")
                "get_closest_marker"))
             #t))
         ;; TODO: Run GUI tests as well
         (replace 'check
           (lambda _
             (substitute* "horizons/constants.py"
               (("IS_DEV_VERSION = False")
                "IS_DEV_VERSION = True"))
             (invoke "pytest" "tests")
             (substitute* "horizons/constants.py"
               (("IS_DEV_VERSION = True")
                "IS_DEV_VERSION = False"))
             #t)))))
    (inputs
     `(("fifengine" ,fifengine)
       ("python:tk" ,python "tk")
       ("python-pillow" ,python-pillow)
       ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     (list intltool
           python-distro
           ;; Required for tests
           python-greenlet
           python-polib
           python-pytest
           python-pytest-mock))
    (home-page "https://unknown-horizons.org/")
    (synopsis "Isometric realtime strategy, economy and city building simulation")
    (description
     "Unknown Horizons is a 2D realtime strategy simulation with an emphasis
on economy and city building.  Expand your small settlement to a strong and
wealthy colony, collect taxes and supply your inhabitants with valuable
goods.  Increase your power with a well balanced economy and with strategic
trade and diplomacy.")
    (license (list
              license:gpl2+        ; Covers code
              license:expat        ; tests/dummy.py, ext/polib.py
              license:cc-by-sa3.0  ; Covers some media content
              license:cc-by3.0     ; Covers some media content
              license:bsd-3))))    ; horizons/ext/speaklater.py

(define-public gnujump
  (package
    (name "gnujump")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnujump/gnujump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05syy9mzbyqcfnm0hrswlmhwlwx54f0l6zhcaq8c1c0f8dgzxhqk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'link-libm
          (lambda _ (setenv "LIBS" "-lm")))
         (add-after 'install 'create-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                 (string-append apps "/gnujump.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=GNUjump~@
                           Comment=Jump up the tower to survive~@
                           Exec=~a/bin/gnujump~@
                           Terminal=false~@
                           Type=Application~@
                           Categories=Game;ArcadeGame~%"
                           out)))))))))
    (inputs
     (list glu mesa sdl sdl-image sdl-mixer))
    (home-page "http://gnujump.es.gnu.org/")
    (synopsis
     "Game of jumping to the next floor, trying not to fall")
    (description
     "GNUjump is a simple, yet addictive game in which you must jump from
platform to platform to avoid falling, while the platforms drop at faster rates
the higher you go.  The game features multiplayer, unlimited FPS, smooth floor
falling, themeable graphics and sounds, and replays.")
    (license license:gpl3+)))

(define-public wesnoth
  (package
    (name "wesnoth")
    (version "1.18.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wesnoth/wesnoth")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0habv0whb0y0r52sjln7yin1nfm3vjjxqlavm7jarcrg2s3v743k"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ;no test target
           #:configure-flags #~'("-DENABLE_SYSTEM_LUA=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'pre-configure
                 (lambda _
                   ;; XXX: Our Lua doesn't have a C++ library, force C linkage.
                   (substitute* '("src/lua/wrapper_lua.h"
                                  "src/lua/wrapper_lualib.h"
                                  "src/lua/wrapper_lauxlib.h")
                     (("#include \"(lua|lualib|lauxlib)\\.h\"")
                      "#include \"lua.hpp\"")))))))
    (inputs
     (list boost
           curl
           dbus
           libvorbis
           lua-5.4
           openssl
           pango
           sdl2
           sdl2-image
           sdl2-mixer))
    (native-inputs
     (list gettext-minimal
           pkg-config
           python-minimal))
    (home-page "https://www.wesnoth.org/")
    (synopsis "Turn-based strategy game")
    (description
     "The Battle for Wesnoth is a fantasy, turn based tactical strategy game,
with several single player campaigns, and multiplayer games (both networked and
local).

Battle for control on a range of maps, using variety of units which have
advantages and disadvantages against different types of attacks.  Units gain
experience and advance levels, and are carried over from one scenario to the
next campaign.")
    (license license:gpl2+)))

(define-public wesnoth-server
  (package
    (inherit wesnoth)
    (name "wesnoth-server")
    (inputs
     (list boost icu4c lua-5.4 openssl))
    (native-inputs
     (list pkg-config))
    (arguments
     (substitute-keyword-arguments (package-arguments wesnoth)
       ((#:configure-flags _)
        #~'("-DENABLE_SYSTEM_LUA=ON" "-DENABLE_GAME=OFF"))))
    (synopsis "Dedicated @emph{Battle for Wesnoth} server")
    (description "This package contains a dedicated server for @emph{The
Battle for Wesnoth}.")))

(define-public gamine
  (package
    (name "gamine")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gamine-game/"
                                  "gamine-" version ".tar.gz"))
              (sha256
               (base32
                "1sc6f4445ciigd6yw0ri92746k4hk6ps0bvj9fm1gbp3c3fslk5n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list bash-minimal
           gstreamer
           gst-plugins-base ; playbin plugin
           gst-plugins-good ; for wav playback
           gtk+))
    (arguments
     `(#:tests? #f
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "SYSCONFDIR=" out "/etc")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'install 'wrap-gamine
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out             (assoc-ref outputs "out"))
                  (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
              (wrap-program (string-append out "/bin/gamine")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))))))))
    (home-page "http://gamine-game.sourceforge.net/")
    (synopsis "Mouse and keyboard discovery for children")
    (description
     "Gamine is a game designed for young children who are learning to use the
mouse and keyboard.  The child uses the mouse to draw colored dots and lines
on the screen and keyboard to display letters.")
    ;; Most files under gpl2+ or gpl3+, but eat.wav under gpl3
    (license license:gpl3)))

(define-public manaplus
  (package
    (name "manaplus")
    (version "2.1.3.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://repo.manaplus.org/manaplus/download/"
                    version "/manaplus-" version ".tar.xz"))
              (sha256
               (base32
                "0ggswsa3xq7lss3j4k7fyzn56sw7hlrwk744i3d9w0n4932nmlg8"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~'("--with-sdl2")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glu curl libxml2 mesa
           sdl2 sdl2-image sdl2-mixer sdl2-net sdl2-ttf))
    (home-page "https://manaplus.org")
    (synopsis "Client for 'The Mana World' and similar games")
    (description
     "ManaPlus is a 2D MMORPG client for game servers.  It is the only
fully supported client for @uref{http://www.themanaworld.org, The mana
world}, @uref{http://evolonline.org, Evol Online} and
@uref{http://landoffire.org, Land of fire}.")
    ;; "src/debug/*" and "src/sdl2gfx/*" are under Zlib.
    ;; "data/themes/{golden-delicious,jewelry}/*" are under CC-BY-SA.
    ;; The rest is under GPL2+.
    (license (list license:gpl2+ license:zlib license:cc-by-sa4.0))))

(define openttd-engine
  (package
    (name "openttd-engine")
    (version "14.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://cdn.openttd.org/openttd-releases/"
                                 version "/openttd-" version "-source.tar.xz"))
             (sha256
              (base32
               "151l05msgfknvfd2pa98n4p01nxw1ainkhc85i7qq5243zqch51c"))))
    (build-system cmake-build-system)
    (inputs
     (list allegro
           fontconfig
           freetype
           icu4c
           libpng
           lzo
           sdl
           xz
           zlib))
    (synopsis "Transportation economics simulator game")
    (description "OpenTTD is a game in which you transport goods and
passengers by land, water and air.  It is a re-implementation of Transport
Tycoon Deluxe with many enhancements including multiplayer mode,
internationalization support, conditional orders and the ability to clone,
autoreplace and autoupdate vehicles.  This package only includes the game
engine.  When you start it you will be prompted to download a graphics set.")
    (home-page "https://www.openttd.org/")
    ;; This package is GPLv2, except for a few files located in
    ;; "src/3rdparty/" which are under the 3-clause BSD, LGPLv2.1+ and Zlib
    ;; licenses.  In addition, this software contains an in-game downloader
    ;; from which the user may find non-functional data licensed under
    ;; different terms.
    (license (list license:bsd-3 license:gpl2 license:lgpl2.1+ license:zlib))))

(define openttd-opengfx
  (package
    (name "openttd-opengfx")
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.openttd.org/opengfx-releases/"
                           version "/opengfx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0nhzlk6s73qvznm5fdwcs1b42g2plf26s5ag39fvck45zm7m48jk"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "INSTALL_DIR="
                             #$output
                             "/share/games/openttd/baseset/opengfx"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              ;; Make sure HOME is writable for GIMP.
              (setenv "HOME" (getcwd))

              (mkdir-p ".local/share")
              (symlink (string-append #$(this-package-native-input "shared-mime-info")
                                      "/share/mime")
                       ".local/share/mime")

              ;; Redirect stdout, not stderr, to /dev/null. This prevents
              ;; dos2unix from receiving its version information as a flag.
              (substitute* "Makefile"
                (("\\$\\(UNIX2DOS\\) -q --version 2>/dev/null")
                 "$(UNIX2DOS) -q --version 1>/dev/null")))))
       ;; The check phase for this package only checks the md5sums of the built
       ;; GRF files against the md5sums of the release versions. Because we use
       ;; different software versions than upstream does, some of the md5sums
       ;; are different. However, the package is still reproducible, it's safe
       ;; to disable this test.
       #:tests? #f
       #:parallel-build? #f))
    (native-inputs
     (list dos2unix
           shared-mime-info
           gimp
           grfcodec
           nml
           which
           python))
    (home-page "http://dev.openttdcoop.org/projects/opengfx")
    (synopsis "Base graphics set for OpenTTD")
    (description
     "The OpenGFX project is an implementation of the OpenTTD base graphics
set that aims to ensure the best possible out-of-the-box experience.

OpenGFX provides you with...
@enumerate
@item All graphics you need to enjoy OpenTTD.
@item Uniquely drawn rail vehicles for every climate.
@item Completely snow-aware rivers.
@item Different river and sea water.
@item Snow-aware buoys.
@end enumerate")
    (license license:gpl2)))

(define openttd-opensfx
  (package
    (name "openttd-opensfx")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.openttd.org/opensfx-releases/"
             version "/opensfx-" version "-source.tar.xz"))
       (sha256
        (base32 "0p336bn6brnbyrf537x36ad9rfz16cjwyzwws4lmfvnql8ycpjj3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list catcodec
           python
           tar))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "DIR_NAME=opensfx")
              (string-append "TAR="
                             (search-input-file %build-inputs "/bin/tar")))
      ;; The check phase only verifies md5sums, see openttd-opengfx.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-reproducible
            (lambda _
              ;; Remove the time dependency of the installed tarball by setting
              ;; the modification times if its members to 0.
              (substitute* "scripts/Makefile.def"
                (("-cf") " --mtime=@0 -cf"))))
          (delete 'configure)
          (add-before 'build 'prebuild
            (lambda _ (invoke "make" "opensfx.cat")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (copy-recursively "opensfx"
                                (string-append (assoc-ref outputs "out")
                                               "/share/games/openttd/baseset"
                                               "/opensfx")))))))
    (home-page "http://dev.openttdcoop.org/projects/opensfx")
    (synopsis "Base sounds for OpenTTD")
    (description "OpenSFX is a set of free base sounds for OpenTTD which make
it possible to play OpenTTD without requiring the proprietary sound files from
the original Transport Tycoon Deluxe.")
    (license license:cc-by-sa3.0)))

(define openttd-openmsx
  (package
    (name "openttd-openmsx")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.openttd.org/openmsx-releases/"
             version "/openmsx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0h583d8fxy78kc3jvpp78r76a48qhxrhm4q7jbnj74aw0kwrcl8g"))))
    (build-system gnu-build-system)
    (native-inputs
     (list grfcodec
           ;; Scripts are Python3 compatible, but call the interpreter as
           ;; python instead of python3.
           python-wrapper
           tar))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "DIR_NAME=openmsx")
              (string-append "TAR="
                             (search-input-file %build-inputs "/bin/tar")))
      ;; The check phase only verifies md5sums, see openttd-opengfx.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (copy-recursively "openmsx"
                                (string-append (assoc-ref outputs "out")
                                               "/share/games/openttd/baseset"
                                               "/openmsx")))))))
    (home-page "http://dev.openttdcoop.org/projects/openmsx")
    (synopsis "Music set for OpenTTD")
    (description "OpenMSX is a music set for OpenTTD which makes it possible
to play OpenTTD without requiring the proprietary music from the original
Transport Tycoon Deluxe.")
    (license license:gpl2)))

(define-public openttd
  (package
    (inherit openttd-engine)
    (name "openttd")
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_BINDIR=" #$output "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sources
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/music/fluidsynth.cpp"
                (("default_sf\\[\\] = \\{" all)
                 (string-append all "
\t/* Guix hardcoded :P */
\t\"" (search-input-file inputs "/share/soundfonts/FreePatsGM.sf2") "\",
")))))
          (add-before 'check 'install-data
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((base "/share/games/openttd"))
                (for-each
                 (lambda (dir)
                   ;; Copy the entire input, so as to not omit documentation
                   ;; etc.
                   (copy-recursively
                    (string-drop-right dir (string-length base))
                    (assoc-ref outputs "out")))
                 (search-path-as-list (list base) (map cdr inputs)))))))))
    (inputs
     (modify-inputs (package-inputs openttd-engine)
       (prepend fluidsynth freepats-gm)))
    (native-inputs
     (modify-inputs (package-native-inputs openttd-engine)
       (prepend openttd-opengfx openttd-openmsx openttd-opensfx)))))

(define openrct2-title-sequences
  (package
   (name "openrct2-title-sequences")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/title-sequences/releases/download/v"
                         version "/title-sequence-v" version ".zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32
       "0qbyxrsw8hlgaq0r5d7lx7an3idy4qbfv7yiw9byhldk763n9cfw"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-title-sequences (string-append out
                                         "/share/openrct2/title-sequences"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (search-input-file %build-inputs "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-title-sequences)
          (copy-recursively "."
                            openrct2-title-sequences)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Title sequences for OpenRCT2")
   (description
    "openrct2-title-sequences is a set of title sequences for OpenRCT2.")
   (license license:gpl3+)))

(define openrct2-objects
  (package
   (name "openrct2-objects")
   (version "1.0.20")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/objects/releases/download/v"
                         version "/objects.zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32 "1q7a38kcwrfijav6app1gf253yfv8b0rljbkah8040y6i7snw9mw"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-objects (string-append out
                                         "/share/openrct2/objects"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (search-input-file %build-inputs "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-objects)
          (copy-recursively "."
                            openrct2-objects)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Objects for OpenRCT2")
   (description
    "openrct2-objects is a set of objects for OpenRCT2.")
   (license license:gpl3+)))

(define-public openquest
  (package
    (name "openquest")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AlbanBedel/scummc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yyq05kfmvgx5aa68kg1l5a4lpsky7hzxxcdvv2xbgf0jljdcl3k"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (substitute* "configure"
                     (("\\|alpha" all)
                      (string-append all "|arm|aarch64|powerpc64le")))
                   (substitute* "examples/example.mak"
                     (("scost.*\n$") "scost\n")
                     (("bmp \\$\\(.*\n$") "bmp\n")
                     (("/%.scc.*\n$") "/%.scc\n")
                     (("voc \\$\\(.*\n$") "voc\n"))
                   (substitute* "Makefile.target"
                     (("distrib-data:.*\n") "distrib-data:\n")
                     (("cp.*/bin" all)
                      (string-append all " || true")))))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments scummc)
       ((#:make-flags _)
        #~(list "SCC=scc"
                "SLD=sld"
                "COST=cost"
                "CHAR=char"
                "SOUN=soun"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda* (#:key make-flags #:allow-other-keys)
                (with-directory-excursion "examples/openquest"
                  (apply invoke "make" "tentacle" make-flags))))
            (add-after 'install 'install-executable
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Create standalone executable.
                (let* ((bash (search-input-file inputs "/bin/bash"))
                       (share (string-append #$output "/examples/openquest"))
                       (scummvm (search-input-file inputs "/bin/scummvm"))
                       (bin (string-append #$output "/bin"))
                       (executable (string-append bin "/openquest")))
                  (mkdir-p bin)
                  (with-output-to-file executable
                    (lambda ()
                      (format #t "#!~a~%" bash)
                      (format #t
                              "exec ~a --path=~a tentacle~%"
                              scummvm share)))
                  (chmod executable #o755))))
            (add-after 'install-executable 'install-desktop-file
              (lambda _
                ;; Create desktop file.  There is no official icon,
                ;; but the main character of the game is a good choice.
                (let* ((apps (string-append #$output "/share/applications"))
                       (share (string-append #$output "/examples/openquest")))
                  (mkdir-p apps)
                  (make-desktop-entry-file
                   (string-append apps "/openquest.desktop")
                   #:name "OpenQuest"
                   #:generic-name "OpenQuest"
                   #:exec (string-append #$output "/bin/openquest")
                   #:icon (string-append share "/graphics/zob/frames/stand_S.bmp")
                   #:categories '("AdventureGame" "Game" "RolePlaying")
                   #:keywords '("game" "adventure" "roleplaying" "2D" "sci-fi")
                   #:comment '((#f "Simple 2D point and click adventure game"))))))))))
    (inputs
     (list bash scummvm))
    (native-inputs
     (modify-inputs (package-native-inputs scummc)
       (prepend scummc)))
    (home-page "https://www.scummvm.org")
    (synopsis "Simple 2D point and click adventure game")
    (description "OpenQuest is a two room adventure game
that follows two aliens who come to Earth in search of a stolen artifact.")
    (license license:gpl2+)))

(define-public openrct2
  (package
    (name "openrct2")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenRCT2/OpenRCT2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01nanpbz5ycdhkyd46fjfvj18sw729l4vk7xg12600f9rjngjk76"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DDOWNLOAD_OBJECTS=OFF"
                               "-DDOWNLOAD_TITLE_SEQUENCES=OFF")
       #:tests? #f                      ; tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-usr-share-paths&add-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((titles (assoc-ref inputs "openrct2-title-sequences"))
                   (objects (assoc-ref inputs "openrct2-objects")))
               ;; Fix some references to /usr/share.
               (substitute* "src/openrct2/platform/Platform.Linux.cpp"
                 (("/usr/share")
                  (string-append (assoc-ref %outputs "out") "/share")))
               (copy-recursively
                (string-append titles "/share/openrct2/title-sequences")
                "data/title")
               (copy-recursively
                (string-append objects "/share/openrct2/objects")
                "data/object"))))
         (add-before 'configure 'get-rid-of-errors
           (lambda _
             ;; Don't treat warnings as errors.
             (substitute* "CMakeLists.txt"
               (("-Werror") ""))
             #t)))))
    (inputs `(("curl" ,curl)
              ("duktape" ,duktape)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("icu4c" ,icu4c)
              ("jansson" ,jansson)
              ("nlohmann-json" ,nlohmann-json)
              ("libpng" ,libpng)
              ("libzip" ,libzip)
              ("mesa" ,mesa)
              ("openrct2-objects" ,openrct2-objects)
              ("openrct2-title-sequences" ,openrct2-title-sequences)
              ("openssl" ,openssl)
              ("sdl2" ,sdl2)
              ("speexdsp" ,speexdsp)
              ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/OpenRCT2/OpenRCT2")
    (synopsis "Free software re-implementation of RollerCoaster Tycoon 2")
    (description "OpenRCT2 is a free software re-implementation of
RollerCoaster Tycoon 2 (RCT2).  The gameplay revolves around building and
maintaining an amusement park containing attractions, shops and facilities.

Note that this package does @emph{not} provide the game assets (sounds,
images, etc.)")
    ;; See <https://github.com/OpenRCT2/OpenRCT2/wiki/Required-RCT2-files>
    ;; regarding assets.
    (license license:gpl3+)))

(define-public openriichi
  (package
    (name "openriichi")
    (version "0.2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FluffyStuff/OpenRiichi")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x6m4mli92chns5dky9aq9w4r4pnycvlpa2q0giydapm5q9fkslf"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags (list "--buildtype=release")
       #:glib-or-gtk? #t))
    (inputs (list glew
                  gtk+
                  libgee
                  sdl2
                  sdl2-image
                  sdl2-mixer))
    (native-inputs (list pkg-config vala))
    (home-page "https://github.com/FluffyStuff/OpenRiichi")
    (synopsis "Japanese Mahjong client")
    (description
     "OpenRiichi is a client for playing Japanese Mahjong, and it supports
singleplayer and multiplayer, with or without bots.  It features all the
standard riichi rules, as well as some optional ones.  It also supports game
logging, so games can be viewed again.")
    (license license:gpl3)))

(define-public pinball
  (package
    (name "pinball")
    (version "0.3.20230219")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/adoptware/pinball")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "02by4df9hgda5zhl9p3rwg0s4mlxdr0v8f8dk152vjp43p1wqvfp"))
             (patches (search-patches "pinball-system-ltdl.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (inputs
     (list glu
           libltdl
           mesa
           (sdl-union (list sdl2 sdl2-image sdl2-mixer))))
    (arguments
     (list
      #:configure-flags
      ;; Configure tries to use pkg-config, but falls short, so:
      #~(list (string-append "CPPFLAGS=-I"
                             #$(this-package-input "sdl-union")
                             "/include/SDL2"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            ;; The `bootstrap` script tries to call a script with
            ;; `/usr/bin/make` in the shebang, but ultimately does the same as
            ;; autoreconf would do, so just use that.
            (lambda _
              (symlink "README.md" "README")
              (invoke "autoreconf" "-vif"))))))
    (home-page "https://pinball.sourceforge.net")
    (synopsis "Pinball machine simulator")
    (description "The Emilia Pinball Project is a pinball simulator.  There
are only two levels to play with, but they are very addictive.")
    (license license:gpl2)))

(define-public pioneers
  (package
    (name "pioneers")
    (version "15.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/pio/"
                                  "pioneers-" version ".tar.gz"))
              (sha256
               (base32
                "07b3xdd81n8ybsb4fzc5lx0813y9crzp1hj69khncf4faj48sdcs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fixes https://issues.guix.gnu.org/47131.
         (add-after 'unpack 'patch-beep-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "client/gtk/audio.c"
               (("\"beep\"")
                (string-append "\"" (assoc-ref inputs "beep") "/bin/beep\"")))
             #t)))))
    (inputs (list avahi beep gtk+ (librsvg-for-system)))
    (native-inputs (list intltool itstool libxml2 pkg-config))
    (synopsis "Board game inspired by The Settlers of Catan")
    (description "Pioneers is an emulation of the board game The Settlers of
Catan.  It can be played on a local network, on the internet, and with AI
players.")
    (home-page "https://pio.sourceforge.net/")
    (license license:gpl2+)))

(define-public einstein
  (package
    (name "einstein")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/e/"
                                  "einstein/einstein_2.0.dfsg.2.orig.tar.gz"))
              (sha256
               (base32
                "1hxrlv6n8py48j487i6wbb4n4vd55w0na69r7ccmmr9vmrsw5mlk"))
              (patches (search-patches "einstein-build.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     (list font-dejavu))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (dejavu (search-input-file
                           inputs "/share/fonts/truetype/DejaVuSans.ttf")))
              (substitute* "Makefile"
                (("PREFIX=/usr/local") (string-append "PREFIX=" out)))
              ;; The patch above registers a free font for use by the binary,
              ;; but the font is copied during the compile phase into a
              ;; resources file, so we need to make the ttf file available.
              (symlink dejavu "res/DejaVuSans.ttf")
              #t))))))
    (synopsis "Logic puzzle game")
    (description "The goal of this logic game is to open all cards in a 6x6
grid, using a number of hints as to their relative position.  The game idea
is attributed to Albert Einstein.")
    ;; The original home page has disappeared.
    (home-page (string-append "http://web.archive.org/web/20120521062745/"
                              "http://games.flowix.com/en/index.html"))
    ;; License according to
    ;; http://web.archive.org/web/20150222180355/http://www.babichev.info/en/projects/index.html
    ;; The source code is a DFSG-sanitized tarball and does not contain any
    ;; license information.
    (license license:gpl3+)))

(define-public powwow
  (package
    (name "powwow")
    (version "1.2.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.hoopajoo.net/static/projects/powwow-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wkl6j91pp40aps2hhnsv0bndgq49smfffws4hqcn7847bpnwwm6"))))
    (inputs
     (list ncurses))
    (build-system gnu-build-system)
    (home-page "https://www.hoopajoo.net/projects/powwow.html")
    (synopsis "MUD and telnet client")
    (description
     "POWWOW is a client software which can be used for telnet as well as for
@dfn{Multi-User Dungeon} (MUD).  Additionally it can serve as a nice client for
the chat server psyced with the specific config located at
http://lavachat.symlynx.com/unix/")
    (license license:gpl2+)))

(define-public red-eclipse
  (let ((release "2.0.0")
        (revision 0))
    (package
      (name "red-eclipse")
      (version (if (zero? revision)
                   release
                   (string-append release "-"
                                  (number->string revision))))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/redeclipse/base")
               (commit (string-append "v" release))
               (recursive? #t))) ; for game data
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sz0mqhwx8r9n4mk3qrxw420nlsm3y0n48gd0lazgd64lfqjh3ab"))
         (modules '((guix build utils)))
         (snippet
          ;; Remove proprietary libraries and other pre-compiled binaries.
          '(begin
             (delete-file-recursively "bin")
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f            ; no check target
         #:make-flags (list "CC=gcc" "-Csrc"
                            (string-append "INSTDIR="
                                           (assoc-ref %outputs "out") "/bin")
                            (string-append "prefix="
                                           (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'add-store-data-package-path-as-default
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/engine/server.cpp"
                 (("data = \"data\"")
                  (string-append "data = \""
                                 (assoc-ref outputs "out")
                                 "/share/redeclipse/data\"")))))
           (delete 'configure)  ; no configure script
           (add-after 'set-paths 'set-sdl-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (search-input-directory inputs "/include/SDL2")
                        ":" (or (getenv "CPATH") "")))))
           (add-after 'install 'copy-data
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "config"
                                   (string-append out "/config"))
                 (copy-file "doc/examples/servinit.cfg"
                            (string-append out "/config/servinit.cfg"))
                 (copy-recursively "data"
                                   (string-append out "/share/redeclipse/data"))
                 (mkdir-p (string-append out "/lib/redeclipse"))
                 (symlink (string-append out "/share/redeclipse/data")
                          (string-append out "/lib/redeclipse/data")))))
           (add-after 'copy-data 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (with-directory-excursion bin
                   (rename-file "redeclipse_linux"
                                ".redeclipse_linux-real")
                   (rename-file "redeclipse_server_linux"
                                ".redeclipse_server_linux-real")
                   (call-with-output-file "redeclipse_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (call-with-output-file "redeclipse_server_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_server_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (chmod "redeclipse_linux" #o555)
                   (chmod "redeclipse_server_linux" #o555))))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list bash-minimal curl freetype glu
             (sdl-union (list sdl2 sdl2-image sdl2-mixer))))
      (home-page "https://redeclipse.net/")
      (synopsis "Arena shooter derived from the Cube 2 engine")
      (description
       "Red Eclipse is an arena shooter, created from the Cube2 engine.
Offering an innovative parkour system and distinct but all potent weapons,
Red Eclipse provides fast paced and accessible gameplay.")
      ;; The engine is under Zlib; data files are covered by the other
      ;; licenses.  More details at file:///doc/all-licenses.txt.
      (license (list license:expat
                     license:zlib
                     license:cc-by-sa4.0
                     license:cc-by-sa3.0
                     license:cc-by3.0
                     license:cc0
                     license:public-domain
                     license:silofl1.1)))))

(define-public grue-hunter
  (package
    (name "grue-hunter")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://jxself.org/grue-hunter.tar.gz"))
              (sha256
               (base32
                "1hjcpy5439qs3v2zykis7hsi0i17zjs62gks3zd8mnfw9ni4i2h3"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (arguments `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (use-modules (srfi srfi-1))

                   (let* ((tarball (assoc-ref %build-inputs "tarball"))
                          (perl    (string-append (assoc-ref %build-inputs
                                                             "perl")
                                                  "/bin"))
                          (gzip    (string-append (assoc-ref %build-inputs
                                                             "gzip")
                                                  "/bin/gzip"))
                          (tar     (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                          (out     (assoc-ref %outputs "out"))
                          (bin     (string-append out "/bin"))
                          (doc     (string-append out
                                                  "/share/doc/grue-hunter")))
                     (copy-file tarball "grue-hunter.tar.gz")
                     (invoke gzip "-d" "grue-hunter.tar.gz")
                     (invoke tar "xvf" "grue-hunter.tar")

                     (mkdir-p bin)
                     (copy-file "grue-hunter/gh.pl"
                                (string-append bin "/grue-hunter"))
                     (patch-shebang (string-append bin "/grue-hunter")
                                    (list perl))

                     (install-file "grue-hunter/AGPLv3.txt" doc)

                     #t))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (home-page "https://jxself.org/grue-hunter.shtml")
    (synopsis "Text adventure game")
    (description
     "Grue Hunter is a text adventure game written in Perl.  You must make
your way through an underground cave system in search of the Grue.  Can you
capture it and get out alive?")
    (license license:agpl3+)))

(define-public lierolibre
  (package
    (name "lierolibre")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/lierolibre/trunk/"
                                  version "/+download/lierolibre-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1cf1gvsn4qq190lrf9k5bpjnqwlcfw7pajvdnh7z5r4jqw0rsbl9"))
              (patches
               (search-patches "lierolibre-check-unaligned-access.patch"
                               "lierolibre-try-building-other-arch.patch"
                               "lierolibre-remove-arch-warning.patch"
                               "lierolibre-newer-libconfig.patch"
                               "lierolibre-is-free-software.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-compiled files.
                  (delete-file "data/LIERO.CHR")
                  (delete-file "data/LIERO.SND")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list imagemagick pkg-config util-linux sox))
    (inputs
     (list boost libconfig
           (sdl-union (list sdl sdl-image sdl-mixer)) zlib))
    (home-page "https://gitlab.com/lierolibre/lierolibre")
    (synopsis "Old-school earthworm action game")
    (description
     "lierolibre is an earthworm action game where you fight another player
(or the computer) underground using a wide array of weapons.

Features:
@itemize
@item 2 worms, 40 weapons, great playability, two game modes: Kill'em All
and Game of Tag, plus AI-players without true intelligence!
@item Dat nostalgia.
@item Extensions via a hidden F1 menu:
@itemize
@item Replays
@item Game controller support
@item Powerlevel palettes
@end itemize
@item Ability to write game variables to plain text files.
@item Ability to load game variables from both EXE and plain text files.
@item Scripts to extract and repack graphics, sounds and levels.
@end itemize

To switch between different window sizes, use F6, F7 and F8, to switch to
fullscreen, use F5 or Alt+Enter.")
    ;; Code mainly BSD-2, some parts under Boost 1.0. All assets are WTFPL2.
    (license (list license:bsd-2 license:boost1.0 license:wtfpl2))))

(define-public tennix
  (package
    (name "tennix")
    (version "1.3.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://repo.or.cz/tennix.git")
                    (commit (string-append "tennix-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fmg0vw8c2spyxy4k64nwky80jsw9mc3vnlch49q6cagjsg9y8dj"))
              ;; Remove non-free images.
              (modules '((guix build utils)))
              (snippet '(begin
                          (for-each delete-file
                                    '("data/loc_training_camp.png"
                                      "data/loc_austrian_open.png"
                                      "data/loc_olympic_green_tennis.png")) #t))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'locate-install
                     ;; Build process cannot expand "$(INSTALL)" in Makefile.
                     (lambda _
                       (substitute* "makefile"
                         (("^CONFIGURE_OUTPUT :=.*" all)
                          (string-append "INSTALL := install -c\n" all))) #t))
                   (replace 'configure
                     ;; The "configure" script is picky about the arguments it
                     ;; gets.  Call it ourselves.
                     (lambda _
                       (invoke "./configure" "--prefix"
                               (assoc-ref %outputs "out")))))))
    (native-inputs (list which))
    (inputs (list python
                  (sdl-union (list sdl2
                                   sdl2-image
                                   sdl2-mixer
                                   sdl2-ttf
                                   sdl2-net
                                   sdl2-gfx))))
    (home-page "https://icculus.org/tennix/")
    (synopsis "Play tennis against the computer or a friend")
    (description
     "Tennix is a 2D tennis game.  You can play against the
computer or against another player using the keyboard.  The game runs
in-window at 640x480 resolution or fullscreen.")
    ;; Project is licensed under GPL2+ terms.  It includes images
    ;; released under Public Domain terms.
    (license (list license:gpl2+ license:public-domain))))

(define-public warzone2100
  (package
    (name "warzone2100")
    (version "4.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/warzone2100/releases/"
                           version
                           "/warzone2100_src.tar.xz"))
       (sha256
        (base32 "1hq56hm6bn3s2pksznh5g8hgq6ww6fnl1pspr3bi93k3z7v0imh1"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "3rdparty"
             (for-each
              delete-file-recursively
              '("discord-rpc"
                "miniupnp"
                "utfcpp")))
             #t))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~'("-DWZ_DISTRIBUTOR=Guix"
                                 "-DWZ_ENABLE_BACKEND_VULKAN=off"
                                 "-DENABLE_DISCORD=off")
           #:tests? #f ; TODO: Tests seem to be broken, configure.ac is missing.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-utfcpp-include
                 (lambda _
                   (substitute* "lib/framework/wzstring.cpp"
                     (("<utfcpp/source/utf8.h>")
                      "<utf8cpp/utf8.h>"))))
               (add-after 'unpack 'link-tests-with-qt
                 (lambda _
                   (substitute* "tests/Makefile.am"
                     (("(framework_linktest_LDADD|maptest_LDADD) = "
                       prefix)
                      (string-append prefix "$(QT5_LIBS) ")))))
               (add-after 'unpack 'fix-ivis-linktest
                 (lambda _
                   (substitute* "tests/ivis_linktest.cpp"
                     (("iV_DrawTextRotated.*;")
                      (string-append
                       "iV_DrawTextRotated(\"Press ESC to exit.\", "
                       "100, 100, 0.0f, font_regular);"))))))))
    (native-inputs (list asciidoc
                     ruby-asciidoctor
                     gettext-minimal
                     pkg-config
                     unzip
                     ;; 7z is used to create .zip archive, not `zip' as in version 3.2.*.
                     p7zip))
    (inputs (list opus
                  curl
                  fontconfig
                  freetype
                  glew
                  harfbuzz
                  libtheora
                  libvorbis
                  libxrandr
                  libsodium
                  miniupnpc
                  openal
                  physfs
                  qtbase-5
                  qtscript
                  openssl
                  sdl2
                  sqlite
                  utfcpp))
    (home-page "https://wz2100.net")
    (synopsis "3D Real-time strategy and real-time tactics game")
    (description
     "Warzone 2100 offers campaign, multi-player, and single-player skirmish
modes.  An extensive tech tree with over 400 different technologies, combined
with the unit design system, allows for a wide variety of possible units and
tactics.")
    ;; Everything is GPLv2+ unless otherwise specified in COPYING.NONGPL
    (license (list license:bsd-3
                   license:cc0
                   license:cc-by-sa3.0
                   license:expat
                   license:gpl2+
                   license:lgpl2.1+))))

(define-public widelands
  (package
    (name "widelands")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/widelands/widelands")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m9hn1sh1siggribzsq79k7p0lggdw41ji7zdl6h648cjak9mdsp"))
       (modules '((guix build utils)))
       (snippet
        #~(delete-file-recursively "src/third_party/minizip"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let ((share (string-append #$output "/share/widelands")))
          (list (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                (string-append "-DWL_INSTALL_BINDIR=" #$output "/bin")
                (string-append "-DWL_INSTALL_BASEDIR=" share)
                (string-append "-DWL_INSTALL_DATADIR=" share)
                "-DOPTION_BUILD_WEBSITE_TOOLS=OFF"
                ;; CMakeLists.txt does not handle properly RelWithDebInfo build
                ;; type.  When used, no game data is installed!
                "-DCMAKE_BUILD_TYPE=Release"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-fonts
            ;; Unbundle fonts already packaged in Guix.  XXX: missing fonts are
            ;; amiri, Culmus, mmrCensus, Nakula, and Sinhala.
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (font)
                 (let* ((path (string-append "share/fonts/truetype/" (basename font)))
                        (target (false-if-exception (search-input-file inputs path))))
                   (when target
                     (delete-file font)
                     (symlink target font))))
               (find-files "data/i18n/fonts" "\\.tt[cf]$")))))))
    (native-inputs
     (list gettext-minimal pkg-config python))
    (inputs
     (list asio
           font-dejavu
           font-wqy-microhei
           glew
           icu4c
           libpng
           minizip
           sdl2
           sdl2-image
           sdl2-mixer
           sdl2-ttf
           zlib))
    (home-page "https://www.widelands.org")
    (synopsis "Fantasy real-time strategy game")
    (description
     "In Widelands, you are the regent of a small clan.  You start out with
nothing but your headquarters, where all your resources are stored.

In the course of the game, you will build an ever growing settlement.  Every
member of your clan will do his or her part to produce more resources---wood,
food, iron, gold and more---to further this growth.  The economic network is
complex and different in the five tribes (Barbarians, Empire, Atlanteans,
Frisians and Amazons).

As you are not alone in the world, you will meet other clans sooner or later.
Some of them may be friendly and you may eventually trade with them.  However,
if you want to rule the world, you will have to train soldiers and fight.

Widelands offers single-player mode with different campaigns; the campaigns
all tell stories of tribes and their struggle in the Widelands universe!
However, settling really starts when you unite with friends over the Internet
or LAN to build up new empires together---or to crush each other in the dusts
of war.  Widelands also offers an Artificial Intelligence to challenge you.")
    ;; Game is released as GPL2+.  Some parts, e.g., art, are released under
    ;; different licenses.
    (license (list license:gpl2+
                   license:expat           ;src/third_party/eris
                   license:silofl1.1       ;Widelands.ttf
                   license:cc-by-sa3.0)))) ;some music files

(define-public widelands-21
  (package
    (inherit widelands)
    (version "21")
    (properties `((superseded . ,widelands)))))

(define-public starfighter
  (package
    (name "starfighter")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pr-starfighter/starfighter/releases"
                    "/download/v" version "/starfighter-"
                    version "-src.tar.gz"))
              (sha256
               (base32
                "0ips79j3sdy8wa64jqka0skbbqkzqiln9bbiiilh4z717q7vz9r5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list pango sdl2 sdl2-image sdl2-mixer sdl2-ttf))
    (home-page "https://pr-starfighter.github.io/")
    (synopsis "2D scrolling shooter game")
    (description
     "In the year 2579, the intergalactic weapons corporation, WEAPCO, has
dominated the galaxy.  Guide Chris Bainfield and his friend Sid Wilson on
their quest to liberate the galaxy from the clutches of WEAPCO.  Along the
way, you will encounter new foes, make new allies, and assist local rebels
in strikes against the evil corporation.")
    ;; gfx and music are under CC-BY 3.0, CC-BY-SA 3.0, CC0 or Public Domain.
    (license (list license:gpl3+
                   license:cc-by3.0
                   license:cc-by-sa3.0
                   license:cc0
                   license:public-domain))))

(define-public chromium-bsu
  (package
    (name "chromium-bsu")
    (version "0.9.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/chromium-bsu"
                                  "/Chromium B.S.U. source code/"
                                  "chromium-bsu-" version ".tar.gz"))
              (sha256
               (base32
                "0jk2w5b6s6nkzri585bbz16cif2fhqcnl5l1mq3rd98r9nil3hd1"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gettext-minimal glu quesoglc
                  (sdl-union (list sdl sdl-image sdl-mixer))))
    (home-page "https://chromium-bsu.sourceforge.net/")
    (synopsis "Fast-paced, arcade-style, top-scrolling space shooter")
    (description
     "In this game you are the captain of the cargo ship Chromium B.S.U. and
are responsible for delivering supplies to the troops on the front line.  Your
ship has a small fleet of robotic fighters which you control from the relative
safety of the Chromium vessel.")
    ;; Clarified Artistic License for everything but sound, which is covered
    ;; by the Expat License.
    (license (list license:clarified-artistic license:expat))))

(define-public tuxemon
  (let ((commit "19708725f8b2d939e39b726f49a8a078eba8bf32")
        (revision "0"))
  (package
    (name "tuxemon")
    (version (git-version "0.4.34" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Tuxemon/Tuxemon")
              (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jdhk6xs4895gifxlkaxk7625wvw2yl1yc6ciay137dk78amhp86"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (substitute* "requirements.txt"
              (("pygame-ce") "pygame") ; The pygame-ce fork isn't packaged in Guix
              (("pygame-menu-ce") "pygame-menu")
              (("==") ">="))
            (substitute* "tuxemon/constants/paths.py"
              (("LIBDIR, ....,") "LIBDIR,"))))))
    (build-system python-build-system)
    (native-inputs (list python-flit-core python-setuptools))
    (propagated-inputs
     (list python-babel
           python-cbor
           python-neteria
           python-natsort
           python-pygame
           python-pyscroll
           python-pytmx
           python-pillow
           python-prompt-toolkit
           python-pydantic-2
           python-pygame-menu
           python-pyyaml
           python-requests))
    (arguments
     (list #:tests? #f ; Tests won't be updated until the API stabilises
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-mods
                 (lambda _
                   (let ((site (string-append #$output "/lib/python"
                                              #$(version-major+minor
                                                 (package-version python))
                                              "/site-packages/tuxemon/mods")))
                     (mkdir-p site)
                     (copy-recursively "mods" site)))))))
    (home-page "https://www.tuxemon.org/")
    (synopsis "Monster-fighting RPG")
    (description
     "Tuxemon is a monster-fighting RPG.
In the spirit of other clones like SuperTux and SuperTuxKart,
Tuxemon aims to create a game with its own unique style
that sets it apart from other monster fighting RPGs.")
    (license license:gpl3+))))

(define-public tuxpaint
  (package
    (name "tuxpaint")
    (version "0.9.23")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint/"
                           version "/tuxpaint-" version ".tar.gz"))
       (sha256
        (base32
         "09k9pxi88r3dx6dyjwf9h85d4qpva4i29qz63dc558hg9v21k69l"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove win32 directory which contains binary dll's and the
           ;; deprecated visualc directory.
           (for-each delete-file-recursively '("win32" "visualc"))
           (substitute* "Makefile"
             ;; Do not rely on $(GPERF) being an absolute file name
             (("\\[ -x \\$\\(GPERF\\) \\]")
              "$(GPERF) --version >/dev/null 2>&1"))
           #t))
       (patches (search-patches "tuxpaint-stamps-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gperf pkg-config))
    (inputs
     (list bash-minimal
           cairo
           fribidi
           gettext-minimal
           libpng
           (librsvg-for-system)
           libpaper
           netpbm
           (sdl-union (list sdl sdl-mixer sdl-ttf sdl-image))))
    ;; TODO: Use system fonts rather than those in data/fonts
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-02"
                      "GPERF=gperf" "CC=gcc"
                      "SDL_PCNAME=sdl SDL_image SDL_mixer SDL_ttf"
                      ,(string-append "PREFIX=" %output)
                      "KDE_PREFIX=$(PREFIX)/share/applications"
                      "KDE_ICON_PREFIX=$(PREFIX)/share/icons/"
                      "COMPLETIONDIR=$(PREFIX)/etc/bash_completion.d")
       #:parallel-build? #f             ;fails on some systems
       #:tests? #f                      ;No tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'no-sys-cache
                    (lambda _           ;do not rebuild system conf cache
                      (substitute* "Makefile"
                        (("kbuildsycoca4") ""))))
                  (add-after 'install 'fix-import
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (net (assoc-ref inputs "netpbm"))
                             (tpi (string-append out "/bin/tuxpaint-import")))
                        (substitute* tpi
                          ;; Point to installation prefix so that the default
                          ;; configure file is found.
                          (("/usr/local") out))
                        ;; tuxpaint-import uses a bunch of programs from
                        ;; netpbm, so make sure it knows where those are
                        (wrap-program tpi
                          `("PATH" ":" prefix
                            (,(string-append net "/bin"))))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "TUXPAINT_STAMPS_PATH")
            (files '("share/tuxpaint/stamps")))))
    (home-page "http://www.tuxpaint.org")
    (synopsis "Drawing software for children")
    (description
     "Tux Paint is a free drawing program designed for young children (kids
ages 3 and up).  It has a simple, easy-to-use interface; fun sound effects;
and an encouraging cartoon mascot who helps guide children as they use the
program.  It provides a blank canvas and a variety of drawing tools to help
your child be creative.")
    (license license:gpl2+)))

(define-public tuxpaint-stamps
  (package
    (name "tuxpaint-stamps")
    (version "2018.09.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-stamps/"
                           (string-map (λ (x) (if (eq? x #\.) #\- x)) version)
                           "/tuxpaint-stamps-" version ".tar.gz"))
       (sha256
        (base32
         "1skr23k27yj3vgwfazpzxp90lb2a278gxrkr3bxw7az6zpkmb3yp"))))
    (build-system trivial-build-system)
    (native-inputs
     (list tar gzip))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (setenv "PATH"
                           (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
                   (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                   (chdir (string-append ,name "-" ,version))
                   (let ((dir (string-append %output "/share/tuxpaint/stamps")))
                     (mkdir-p dir)
                     (copy-recursively "stamps" dir))
                   #t)))
    (home-page (package-home-page tuxpaint))
    (synopsis "Stamp images for Tux Paint")
    (description
     "This package contains a set of \"Rubber Stamp\" images which can be used
with the \"Stamp\" tool within Tux Paint.")
    (license license:gpl2+)))

(define-public tuxpaint-config
  (package
    (name "tuxpaint-config")
    (version "0.0.14")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-config/"
                           version "/tuxpaint-config-" version ".tar.gz"))
       (sha256
        (base32
         "0zkgxk436nqcp43zghkfmh397c7dvh5bwn2as7gwvv208bzyij6g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list fltk
           libpaper
           ;; TODO: Should the following be propagated by fltk?
           libx11
           libxft
           mesa))
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-01"
                      "CONFDIR=/etc/tuxpaint" ;don't write to store
                      ,(string-append "PREFIX=" %output)
                      "GNOME_PREFIX=$(PREFIX)")
       #:parallel-build? #f             ;race conditions
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'gzip-no-name
                    (lambda* _
                      (substitute* "Makefile"
                        ;; tuxpaint-config compresses its own documentation;
                        ;; make sure it uses flags for reproducibility.
                        (("gzip") "gzip --no-name"))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        #t))))))
    (home-page (package-home-page tuxpaint))
    (synopsis "Configure Tux Paint")
    (description
     "Tux Paint Config is a graphical configuration editor for Tux Paint.")
    (license license:gpl2)))            ;no "or later" present

(define-public supertux
  (package
   (name "supertux")
   (version "0.6.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/SuperTux/supertux/"
                                "releases/download/v" version "/SuperTux-v"
                                version "-Source.tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "1xkr3ka2sxp5s0spp84iv294i29s1vxqzazb6kmjc0n415h0x57p"))
            (patches
             (search-patches "supertux-unbundle-squirrel.patch"))))
   (arguments
    '(#:tests? #f
      #:configure-flags '("-DINSTALL_SUBDIR_BIN=bin"
                          "-DUSE_SYSTEM_PHYSFS=ON")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-squirrel-path
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((squirrel (assoc-ref inputs "squirrel")))
              (substitute* "CMakeLists.txt"
                (("set\\(SQUIRREL_PREFIX.*")
                 (string-append "set(SQUIRREL_PREFIX " squirrel ")"))
                (("add_dependencies\\(supertux2_lib squirrel\\)") "")
                (("\\$\\{SQUIRREL_PREFIX\\}/include")
                 (string-append "${SQUIRREL_PREFIX}/include/squirrel"))))
            #t)))))
   (build-system cmake-build-system)
   (inputs (list boost
                 curl
                 freetype
                 glew
                 glm
                 libogg
                 libvorbis
                 mesa
                 openal
                 physfs
                 sdl2
                 sdl2-image
                 sdl2-mixer
                 squirrel))
   (native-inputs
    (list pkg-config))
   (synopsis "2D platformer game")
   (description "SuperTux is a classic 2D jump'n run sidescroller game in
a style similar to the original Super Mario games.")
   (home-page "https://supertux.org/")
   (license license:gpl3+)))

(define-public tintin++
  (package
    (name "tintin++")
    (version "2.02.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tintin/TinTin++ Source Code/"
                           (string-drop-right version 1)
                           "/tintin-" version ".tar.gz"))
       (sha256
        (base32 "000sg16w7790ha8ys31qjh1ip5hl02ldfwj1zy6dqz0y5i7zvydn"))))
    (inputs
     (list gnutls pcre readline zlib))
    (arguments
     '(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         ;; The source is in tt/src.
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t)))))
    (build-system gnu-build-system)
    (home-page "https://tintin.mudhalla.net/")
    (synopsis "MUD client")
    (description
     "TinTin++ is a MUD client which supports MCCP (Mud Client Compression
Protocol), MMCP (Mud Master Chat Protocol), xterm 256 colors, most TELNET
options used by MUDs, as well as those required to login via telnet on
Linux / Mac OS X servers, and an auto mapper with a VT100 map display.")
    (license license:gpl3+)))

(define-public laby
  (package
    (name "laby")
    (version "0.7.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/sgimenez/laby")
                    (commit (string-append name "-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1y6nfxcjhqg9bb81hs0wijg7kcwk5kff81rgd8bsv5ps7ia9nj6b"))
             (patches (search-patches "laby-make-install.patch"
                                      "laby-use-tmpdir-from-runtime.patch"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     (list bash-minimal
           gdk-pixbuf
           lablgtk3
           (librsvg-for-system)
           ocaml-lablgtk3-sourceview3
           ocaml
           ocaml-findlib
           ocamlbuild))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'build 'set-library-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((lablgtk #$(this-package-input "lablgtk")))
                     (setenv "LD_LIBRARY_PATH"
                             (string-append lablgtk "/lib/ocaml/stublibs")))))
               (add-after 'glib-or-gtk-wrap 'wrap-pixbuf
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((laby (string-append #$output "/bin/laby")))
                      (wrap-program laby
                        ;; Wrapping GDK_PIXBUF_MODULE_FILE allows laby to
                        ;; function in pure environments.
                        `("GDK_PIXBUF_MODULE_FILE" =
                          (,(getenv "GDK_PIXBUF_MODULE_FILE"))))))))
           #:tests? #f ; no 'check' target
           #:make-flags
           #~(list (string-append "PREFIX=" #$output) "all")))
    (home-page "https://sgimenez.github.io/laby/")
    (synopsis "Programming game")
    (description "Learn programming, playing with ants and spider webs ;-)
Your robot ant can be programmed in many languages: OCaml, Python, C, C++,
Java, Ruby, Lua, JavaScript, Pascal, Perl, Scheme, Vala, Prolog.  Experienced
programmers may also add their own favorite language.")
    (license license:gpl3+)))

(define-public bambam
  (package
    (name "bambam")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/porridge/bambam")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "148cqahklqd4m88j5z1xf3fh4vha41f31wian11hkas106mbsya9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share")))
               (mkdir-p bin)
               (copy-file "bambam.py" (string-append bin "/bambam"))
               (install-file "bambam.6" (string-append share "/man/man6"))
               (copy-recursively "data" (string-append share "/bambam/data")))
             #t)))))
    (inputs
     (list python-pygame))
    (home-page "https://github.com/porridge/bambam")
    (synopsis "Keyboard mashing and doodling game for babies")
    (description "Bambam is a simple baby keyboard (and gamepad) masher
application that locks the keyboard and mouse and instead displays bright
colors, pictures, and sounds.")
    (license license:gpl3+)))

(define-public moonlight-qt
  (package
    (name "moonlight-qt")
    (version "6.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moonlight-stream/moonlight-qt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06fxf3m26k036asxjkkykk5q96nincwmpiqm953m7zgr9224gidx"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* _
              (symlink (string-append
                        #$(this-package-input "sdl2-gamecontrollerdb")
                        "/share/sdl2/gamecontrollerdb.txt")
                       "app/SDL_GameControllerDB/gamecontrollerdb.txt")
              ;; Unbundle libraries.
              (substitute* "moonlight-qt.pro"
                (("    moonlight-common-c.*\n") "")
                (("    qmdnsengine.*\n") "")
                (("    h264bitstream.*\n") "")
                (("    app \\\\") "    app")
                (("app.depends") "INCLUDEPATH +="))
              (invoke "qmake" (string-append "PREFIX=" #$output)))))))
    (native-inputs (list pkg-config qttools-5))
    (inputs (list ffmpeg
                  h264bitstream
                  libva
                  libvdpau
                  moonlight-common
                  openssl
                  opus
                  qmdnsengine
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtwayland-5
                  wayland
                  sdl2
                  sdl2-ttf
                  sdl2-gamecontrollerdb))
    (synopsis "GameStream client")
    (description
     "Moonlight is an implementation of NVIDIA's GameStream, as used by the
NVIDIA Shield.")
    (home-page "https://moonlight-stream.org")
    (license license:gpl3+)))

(define-public moonlight-common
  ;; Used as submodule in https://github.com/moonlight-stream/moonlight
  (let ((commit "8599b6042a4ba27749b0f94134dd614b4328a9bc")
        (revision "1"))
    (package
      (name "moonlight-common")
      (version (git-version "6.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://github.com/moonlight-stream/moonlight-common-c")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19k8rs2p51zs0h3wj22xw8bgj9c0ma0dc6y7qk5pk75p8ymqp9d3"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'use-system-enet-package
                            (lambda _
                              (substitute* "CMakeLists.txt"
                                (("add_subdirectory\\(enet\\)")
                                 ""))))
                          (replace 'install
                            (lambda* (#:key outputs source #:allow-other-keys)
                              (let* ((include (string-append #$output
                                                             "/include"))
                                     (lib (string-append #$output "/lib")))
                                (mkdir-p include)
                                (mkdir-p lib)
                                (install-file (string-append source
                                               "/src/Limelight.h") include)
                                (install-file "libmoonlight-common-c.so" lib)))))))
      (native-inputs (list pkg-config))
      (inputs (list enet-moonlight openssl qtbase-5))
      (synopsis "GameStream protocol core implementation")
      (description
       "This package provides the GameStream core code for the protocol.")
      (home-page "https://github.com/moonlight-stream/moonlight-common-c")
      (license license:gpl3+))))

(define-public mrrescue
  (package
    (name "mrrescue")
    (version "1.02e")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SimonLarsen/mrrescue/releases/"
                    "download/" version "/mrrescue" version ".love"))
              (file-name (string-append name "-" version ".love"))
              (sha256
               (base32
                "0jwzbwkgp1l5ia6c7s760gmdirbsncp6nfqp7vqdqsfb63la9gl2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (script (string-append out "/bin/" ,name))
                (data   (string-append out "/share/" ,name))
                (source (assoc-ref %build-inputs "source"))
                (unzip  (search-input-file %build-inputs "/bin/unzip"))
                (patch  (search-input-file %build-inputs "/bin/patch"))
                (bash   (search-input-file %build-inputs "/bin/bash"))
                (love   (search-input-file %build-inputs "/bin/love")))

           (mkdir-p (dirname script))
           (with-output-to-file script
             (lambda ()
               (format #t "#!~a~%" bash)
               (format #t "exec -a ~a \"~a\" \"~a\"~%" ,name love data)))
           (chmod script #o755)

           ;; The better way to package this game would be to install *only* the
           ;; script above, pointing to the unextracted .love file in the store.
           ;; However, mrrescue 1.02e needs to be patched to work with Love 11.
           ;; Instead of extracting the .love file, patching it, and re-zipping
           ;; it to the store, simply point the script to the extracted patched
           ;; data directory directly.
           (mkdir-p data)
           (with-directory-excursion data
             (invoke unzip source)
             (invoke patch "-p1" "-i"
                     (assoc-ref %build-inputs "love-11.patch")))
           #t))))
    (native-inputs
     `(("unzip" ,unzip)
       ("patch" ,patch)
       ("love-11.patch" ,(search-patch "mrrescue-support-love-11.patch"))))
    (inputs
     (list bash love))
    (home-page "https://tangramgames.dk/games/mrrescue")
    (synopsis "Arcade-style fire fighting game")
    (description
     "Mr. Rescue is an arcade styled 2d action game centered around evacuating
civilians from burning buildings.  The game features fast-paced fire
extinguishing action, intense boss battles, a catchy soundtrack, and lots of
throwing people around in pseudo-randomly generated buildings.")
    (license (list license:zlib             ; for source code
                   license:cc-by-sa3.0))))  ; for graphics and music assets

(define-public hyperrogue
  (package
    (name "hyperrogue")
    (version "12.1a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zenorogue/hyperrogue")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l09d1r3jdwp54zq071fk09hpggif5phjn0gsapzrjy3i289jran"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:make-flags #~(list "HYPERROGUE_USE_GLEW=1"
                           "HYPERROGUE_USE_PNG=1")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'set-sdl-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CPATH"
                      (string-append (or (getenv "CPATH") "") ":"
                                     (search-input-directory inputs
                                                             "/include/SDL")))))
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((share-dir (string-append #$output "/share/hyperrogue/"))
                    (fonts-dir (dirname
                                (search-input-file inputs
                                                   "DejaVuSans-Bold.ttf"))))
                ;; Set fonts and music path.
                (substitute* "sysconfig.h"
                  (("(#define HYPERPATH ).*" _ lead)
                   (string-append lead "\"" share-dir "\"\n"))
                  (("(#define HYPERFONTPATH ).*" _ lead)
                   (string-append lead "\"" fonts-dir "/\"\n")))
                ;; Disable build machine CPU optimizations and warnings treated
                ;; as errors.
                (substitute* "Makefile"
                  (("-march=native") "")
                  (("-Werror") "")))))
          (replace 'install
            (lambda _
              (install-file "hyperrogue" (string-append #$output "/bin"))
              (let ((share-dir (string-append #$output "/share/hyperrogue/")))
                (install-file "hyperrogue-music.txt" share-dir)
                (for-each (lambda (dir)
                            (copy-recursively dir
                                              (string-append share-dir dir)))
                          '("music" "sounds"))))))))
    (inputs
     (list font-dejavu
           glew
           libpng
           (sdl-union (list sdl sdl-gfx sdl-mixer sdl-ttf))))
    (home-page "https://roguetemple.com/z/hyper")
    (synopsis "Non-euclidean graphical rogue-like game")
    (description
     "HyperRogue is a game in which the player collects treasures and fights
monsters -- rogue-like but for the fact that it is played on the hyperbolic
plane and not in euclidean space.

In HyperRogue, the player can move through different parts of the world, which
are home to particular creatures and may be subject to their own rules of
\"physics\".

While the game can use ASCII characters to display the the classical rogue
symbols, it still needs graphics to render the non-euclidean world.")
    (license (list license:bsd-3        ; glew.c, mtrand.*
                   license:cc-by-sa3.0  ; music
                   license:cc-by-sa4.0  ; sounds
                   license:cc0
                   license:public-domain ; direntx.*, some sounds
                   license:zlib          ; savepng.*
                   license:gpl2+))))     ; remaining files

(define-public kobodeluxe
  (package
    (name "kobodeluxe")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://olofson.net/kobodl/download/KoboDeluxe-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0b2wvdpnmaibsy419c16dfwj5kvd3pccby2aaqvm964x74592yqg"))
              (patches (search-patches
                        "kobodeluxe-const-charp-conversion.patch"
                        "kobodeluxe-enemies-pipe-decl.patch"
                        "kobodeluxe-graphics-window-signed-char.patch"
                        "kobodeluxe-manpage-minus-not-hyphen.patch"
                        "kobodeluxe-midicon-segmentation-fault.patch"
                        "kobodeluxe-paths.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (inputs
     (list glu
           (sdl-union (list sdl sdl-image))))
    (synopsis "Shooter with space station destruction")
    (description
     "Kobo Deluxe is an enhanced version of Akira Higuchi's XKobo graphical game
for Un*x systems with X11.")
    (home-page "http://olofson.net/kobodl/")
    (license license:gpl2+)))

(define-public freeciv
  (package
   (name "freeciv")
   (version "3.1.4")
   (source
    (origin
     (method url-fetch)
     (uri (list (string-append
                  "https://files.freeciv.org/stable/freeciv-"
                  version ".tar.xz")
                (string-append
                  "mirror://sourceforge/freeciv/Freeciv%20"
                  (version-major+minor version) "/" version
                  "/freeciv-" version ".tar.xz")))
     (sha256
      (base32 "1r4n6bqvazsn6q41xq5l86xj7rpfi4dxva6mhz17ql640fwrp68l"))))
   (build-system gnu-build-system)
   (inputs
    (list curl cyrus-sasl gtk+ sdl2-mixer sqlite zlib))
   (native-inputs
    (list pkg-config))
   (home-page "https://www.freeciv.org/")
   (synopsis "Turn-based empire building strategy game")
   (description "Freeciv is a turn-based empire building strategy game
inspired by the history of human civilization.  The game commences in
prehistory and your mission is to lead your tribe from the Stone Age
into the Space Age.")
   (license license:gpl2+)))

(define-public no-more-secrets
  (package
    (name "no-more-secrets")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bartobri/no-more-secrets")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zfv4qabikf8w9winsr4brxrdvs3f0d7xvydksyx8bydadsm2v2h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "CC=gcc" "all-ncurses"
                          (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list ncurses))
    (home-page "https://github.com/bartobri/no-more-secrets")
    (synopsis "Recreation of data decryption effect in \"Sneakers\"")
    (description
     "@code{No More Secrets} provides a command line tool called \"nms\"
that recreates the famous data decryption effect seen on screen in the 1992
movie \"Sneakers\".

This command works on piped data.  Pipe any ASCII or UTF-8 text to nms, and
it will apply the hollywood effect, initially showing encrypted data, then
starting a decryption sequence to reveal the original plaintext characters.")
    (license license:expat)))

(define-public megaglest-data
  (package
    (name "megaglest-data")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-data"
             "/releases/download/" version "/megaglest-data-"
             version ".tar.xz"))
       (sha256
        (base32
         "0ipgza33z89fw3si32iafm981f3fvm0zldvbxj29whghd2k3rpj3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://megaglest.org/")
    (synopsis "Data files for MegaGlest")
    (description "This package contains the data files required for MegaGlest.")
    (license license:cc-by-sa3.0)))

(define-public megaglest
  (package
    (name "megaglest")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-source"
             "/releases/download/" version "/megaglest-source-"
             version ".tar.xz"))
       (sha256
        (base32
         "1ffck3ii1wp5k3nn5p0ga06jgp7pzk4zw0xln3xim2w7qrxzdzh9"))))
    (build-system cmake-build-system)
    (inputs
     (list curl
           fontconfig
           ftgl
           glew
           libjpeg-turbo
           megaglest-data
           mesa
           miniupnpc
           openal
           libircclient
           libpng
           libvorbis
           lua
           sdl2
           wxwidgets-3.0))
    (native-inputs
     (list cppunit pkg-config))
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_CXX_FLAGS=-fcommon"
             "-DCMAKE_C_FLAGS=-fcommon"
             (string-append "-DCUSTOM_DATA_INSTALL_PATH="
                            (search-input-directory %build-inputs
                                                    "share/megaglest"))
             "-DBUILD_MEGAGLEST_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-ini-search-path
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "source/glest_game/global/config.cpp"
                        (("/usr/share/megaglest/")
                         (string-append (assoc-ref outputs "out")
                                        "/share/megaglest/"))))))
       #:test-target "megaglest_tests"))
    (home-page "https://megaglest.org/")
    (synopsis "3D real-time strategy (RTS) game")
    (description "MegaGlest is a cross-platform 3D real-time strategy (RTS)
game, where you control the armies of one of seven different factions: Tech,
Magic, Egypt, Indians, Norsemen, Persian or Romans.")
    (license license:gpl2+)))

(define-public freegish
  (let ((commit "caf58a2f990a939230bab82226e29cd79732f366")
        (revision "3"))
    (package
      (name "freegish")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/freegish/freegish")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0980ad8xg0bzm6507bq9sbgw03i7jj33g0f955g0q8jvpb22r65v"))
                (modules '((guix build utils)))
                ;; The audio files in the "music" directory are licensed under
                ;; CC-BY-NC, so we delete them.
                (snippet
                 '(begin
                    (delete-file-recursively "music")
                    #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:configure-flags
         (list "-DINSTALL_FHS=ON")))
      (inputs
       (list sdl2
             openal
             libvorbis
             libogg
             mesa
             libpng))
      (home-page "https://github.com/freegish/freegish")
      (synopsis "Side-scrolling physics platformer with a ball of tar")
      (description "In FreeGish you control Gish, a ball of tar who lives
happily with his girlfriend Brea, until one day a mysterious dark creature
emerges from a sewer hole and pulls her below ground.")
      ;; The textures are available under the Expat license.  All other assets
      ;; (including levels) are covered under CC-BY-SA or public domain.  The
      ;; source code is under GPLv2+.
      (license (list license:gpl2+
                     license:expat
                     license:public-domain
                     license:cc-by-sa3.0)))))

(define-public cdogs-sdl
  (package
    (name "cdogs-sdl")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cxong/cdogs-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1akay3ad2bkiqa7vfkh3qyhiqax8ikp1v6lfjysvxg65wkqdvc"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DCDOGS_DATA_DIR=" #$output
                             "/share/cdogs-sdl/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-install-directory
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set\\(DATA_INSTALL_DIR \".\"\\)")
                 (string-append "set(DATA_INSTALL_DIR \""
                                #$output "/share/cdogs-sdl\")"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ mesa sdl2 sdl2-image sdl2-mixer))
    (home-page "https://cxong.github.io/cdogs-sdl/")
    (synopsis "Classic overhead run-and-gun game")
    (description "C-Dogs SDL is a classic overhead run-and-gun game,
supporting up to 4 players in co-op and deathmatch modes.  Customize your
player, choose from many weapons, and blast, slide and slash your way through
over 100 user-created campaigns.")
    ;; GPLv2+ for code (includes files under BSD-2 and BSD-3),
    ;; CC0/CC-BY/CC-BY-SA for assets.
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:cc-by3.0
                   license:cc-by-sa3.0))))

(define-public kiki
  (package
    (name "kiki")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/kiki/kiki-src/"
                                  version "/kiki-" version "-src.tgz"))
              (sha256
               (base32
                "0ihjdsxbn8z3cz0gpcprafiipcqaiskgdnh1rhmw4qff8dszalbn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.dll$"))
                  #t))
              (patches
               (search-patches "kiki-level-selection-crash.patch"
                               "kiki-makefile.patch"
                               "kiki-missing-includes.patch"
                               "kiki-portability-64bit.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CXX=g++")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append
                      (search-input-directory inputs "include/SDL") ":"
                      (search-input-directory inputs "include/python2.7")
                      ":" (or (getenv "CPLUS_INCLUDE_PATH") "")))
             (substitute* "src/main/main.cpp"
               (("#include <SDL.h>" line)
                (string-append line "
#define K_INCLUDE_GLUT
#include \"KIncludeTools.h\""))
               (("// initialize SDL" line)
                (string-append "glutInit(&argc,argv);\n" line)))
             (substitute* "src/main/KikiController.cpp"
               (("getenv\\(\"KIKI_HOME\"\\)")
                (string-append "\"" (assoc-ref outputs "out") "/share/kiki/\"")))
             (substitute* "linux/Makefile"
               (("CXXOPTS =" line)
                (string-append line " -fpermissive"))
               (("PYTHON_VERSION=.*") "PYTHON_VERSION=2.7")
               (("PYTHONHOME =.*")
                (string-append "PYTHONHOME = "
                               (assoc-ref inputs "python")
                               "/lib/python2.7/"))
               (("\\$\\(GLLIBS\\)" line)
                (string-append line " -lm -lpython2.7")))
             (substitute* "src/main/KikiPythonWidget.h"
               (("#define __KikiPythonWidget" line)
                (string-append line "\n#include \"KikiPython.h\"")))
             #t))
         (add-before 'build 'build-kodilib
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "kodilib/linux"
               (apply invoke "make" make-flags))))
         (add-after 'build-kodilib 'chdir
           (lambda _ (chdir "linux") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/kiki")))
               (mkdir-p bin)
               (mkdir-p share)
               (install-file "kiki" bin)
               (copy-recursively "../py" (string-append share "/py"))
               (copy-recursively "../sound" (string-append share "/sound"))
               #t))))))
    (inputs
     `(("glu" ,glu)
       ;; Kiki builds fine with freeglut 3.0.0 but segfaults on start.
       ("freeglut" ,freeglut-2.8)
       ("libxcrypt" ,libxcrypt)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image)))
       ("python" ,python-2)))
    (native-inputs
     (list swig))
    (home-page "https://kiki.sourceforge.net/")
    (synopsis "3D puzzle game")
    (description "Kiki the nano bot is a 3D puzzle game.  It is basically a
mixture of the games Sokoban and Kula-World.  Your task is to help Kiki, a
small robot living in the nano world, repair its maker.")
    ;; See <http://metadata.ftp-master.debian.org/changelogs/main/k/
    ;; kiki-the-nano-bot/kiki-the-nano-bot_1.0.2+dfsg1-4_copyright>
    ;; for a statement from the author.
    (license license:public-domain)))

(define-public teeworlds
  (package
    (name "teeworlds")
    (version "0.7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/teeworlds/teeworlds")
                    (commit version)
                    ;; There are two submodules in datasrc/{languages,maps}
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l19ksmimg6b8zzjy0skyhh7z11ql7n5gvilkv7ay5x2b9ndbqwz"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (ice-9 regex)
                         (srfi srfi-1)
                         (srfi srfi-26)))
              (snippet ; remove bundled libraries except md5
               '(let ((base-dir "src/engine/external/"))
                  (for-each (compose (cut delete-file-recursively <>)
                                     (cut string-append base-dir <>))
                            (remove (cut string-match "(^.)|(^md5$)" <>)
                                    (scandir base-dir)))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "run_tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Embed path to assets.
             (substitute* "src/engine/shared/storage.cpp"
               (("#define DATA_DIR.*")
                (string-append "#define DATA_DIR \""
                               (assoc-ref outputs "out")
                               "/share/teeworlds/data"
                               "\"")))
             #t))
         (add-after 'unpack 'replace-font
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file "datasrc/fonts/DejaVuSans.ttf")
             (symlink (string-append (assoc-ref inputs "font-dejavu")
                                     "/share/fonts/truetype/DejaVuSans.ttf")
                      "datasrc/fonts/DejaVuSans.ttf")
             #t)))))
    (inputs
     (list freetype
           font-dejavu
           glu
           json-parser
           mesa
           pnglite
           sdl2
           sdl2-image
           sdl2-mixer
           wavpack
           openssl
           zlib))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.teeworlds.com")
    (synopsis "2D retro multiplayer shooter game")
    (description "Teeworlds is an online multiplayer game.  Battle with up to
16 players in a variety of game modes, including Team Deathmatch and Capture
The Flag.  You can even design your own maps!")
    (license (list license:bsd-3 license:cc-by-sa3.0)))) ; game+maps&languages

(define-public enigma
  (package
    (name "enigma")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/enigma-game/"
                                  "Release%20" version "/enigma-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00ffh9pypj1948pg3q9sjp1nmiabh52p5c8wpg9n1dcfgl3cywnq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-system-enet")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-new-gcc
           (lambda _
             ;; Fix build with GCC6 and later by avoiding comparing ifstream
             ;; to NULL.  Can be removed for versions > 1.21.
             (substitute* "src/lev/Proxy.cc"
               (("ifs != NULL")
                "ifs"))
             #t))
         (add-after 'unpack 'find-sdl
           (lambda _
             (substitute* "configure"
               (("SDL_ttf.h") "SDL/SDL_ttf.h"))
             (substitute* '("tools/ttf2bmf.cc"
                            "lib-src/enigma-core/ecl_font.cc"
                            "lib-src/enigma-core/ecl_video.cc"
                            "lib-src/enigma-core/ecl_buffer.hh"
                            "src/SoundEngine.cc"
                            "src/SoundEngine.hh"
                            "src/MusicManager.cc"
                            "src/MusicManager.hh"
                            "src/d_models.cc"
                            "src/main.cc"
                            "src/network.cc")
               (("#include \"SDL_(image|ttf|mixer|types|syswm|mutex).h\"" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             (substitute* "src/main.cc"
               (("#include <SDL_(image|ttf|mixer).h>" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             #t)))))
    (inputs
     (list xerces-c
           (sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)) curl
           enet))
    (native-inputs
     (list pkg-config imagemagick))
    (home-page "https://www.nongnu.org/enigma")
    (synopsis "Puzzle game with a dexterity component")
    (description "Enigma is a puzzle game with 550 unique levels.  The object
of the game is to find and uncover pairs of identically colored ‘Oxyd’ stones.
Simple?  Yes.  Easy?  Certainly not!  Hidden traps, vast mazes, laser beams,
and most of all, countless hairy puzzles usually block your direct way to the
Oxyd stones.  Enigma’s game objects (and there are hundreds of them, lest you
get bored) interact in many unexpected ways, and since many of them follow the
laws of physics (Enigma’s special laws of physics, that is), controlling them
with the mouse isn’t always trivial.")
    (license license:gpl2+)))

(define-public chroma
  (package
    (name "chroma")
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://level7.org.uk/chroma/download/chroma-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "02hn448ckfxbx2fqr9wgf66rwl0vr4gl87yvsr5fc99zz9zw2f5v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))
       ("freetype" ,freetype)
       ("ncurses" ,ncurses)
       ("fontconfig" ,fontconfig)
       ("libxft" ,libxft)))
    (native-inputs
     (list pkg-config))
    (home-page "http://level7.org.uk/chroma/")
    (synopsis "Abstract puzzle game")
    (description "Chroma is an abstract puzzle game.  A variety of colourful
shapes are arranged in a series of increasingly complex patterns, forming
fiendish traps that must be disarmed and mysterious puzzles that must be
manipulated in order to give up their subtle secrets.  Initially so
straightforward that anyone can pick it up and begin to play, yet gradually
becoming difficult enough to tax even the brightest of minds.")
    (license license:gpl2+)))

(define-public fillets-ng
  (package
    (name "fillets-ng")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fillets/"
                                  "Fish%20Fillets%20-%20Next%20Generation/"
                                  version "/fillets-ng-" version ".tar.gz"))
              (sha256
               (base32
                "1nljp75aqqb35qq3x7abhs2kp69vjcj0h1vxcpdyn2yn2nalv6ij"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua="
                            (assoc-ref %build-inputs "lua")))
       #:make-flags
       (list (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL")
             (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))
       #:phases
       (modify-phases %standard-phases
         ;; Lua 5.1 does not provide it.
         (add-after 'unpack 'do-not-link-with-lualib
           (lambda _
             (substitute* "configure"
               (("-llualib") ""))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((data (string-append (assoc-ref outputs "out")
                                        "/share/games/fillets-ng")))
               (mkdir-p data)
               (invoke "tar" "-xvf"
                       (assoc-ref inputs "fillets-ng-data")
                       "--strip-components=1"
                       "-C" data)))))))
    (inputs
     (list (sdl-union (list sdl sdl-mixer sdl-image sdl-ttf)) fribidi
           libx11 lua-5.1))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("fillets-ng-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/fillets/"
                               "Fish%20Fillets%20-%20Next%20Generation/"
                               version "/fillets-ng-data-" version ".tar.gz"))
           (sha256
            (base32
             "169p0yqh2gxvhdilvjc2ld8aap7lv2nhkhkg4i1hlmgc6pxpkjgh"))))))
    (home-page "https://fillets.sourceforge.net")
    (synopsis "Puzzle game")
    (description "Fish Fillets NG is strictly a puzzle game.  The goal in
every of the seventy levels is always the same: find a safe way out.  The fish
utter witty remarks about their surroundings, the various inhabitants of their
underwater realm quarrel among themselves or comment on the efforts of your
fish.  The whole game is accompanied by quiet, comforting music.")
    (license license:gpl2+)))

(define-public crawl
  (package
    (name "crawl")
    (version "0.32.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/crawl/crawl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bdy1gdp0hx9ypj61jvd19wrfn0ilbs682nck0ld9nc0rw5wa64f"))
       (patches (search-patches "crawl-upgrade-saves.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list lua-5.1 ncurses sqlite zlib))
    (native-inputs
     (list bash-minimal
           bison
           flex
           perl
           pkg-config
           python-wrapper
           python-pyyaml))
    (arguments
     (list
      #:make-flags
      #~(list (string-append "SQLITE_INCLUDE_DIR="
                             #$(this-package-input "sqlite")
                             "/include")
              (string-append "prefix=" #$output)
              "SAVEDIR=~/.crawl"
              ;; Don't compile with SSE on systems which don't have it.
              #$@(match (%current-system)
                   ((or "i686-linux" "x86_64-linux")
                    '())
                   (_ '("NOSSE=TRUE")))
              ;; don't build any bundled dependencies
              "BUILD_LUA="
              "BUILD_SQLITE="
              "BUILD_ZLIB="
              "-Ccrawl-ref/source")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (call-with-output-file "crawl-ref/source/util/release_ver"
                (lambda (port)
                  (display #$version port)))))
          (add-after 'unpack 'find-SDL-image
            (lambda _
              (substitute* "crawl-ref/source/windowmanager-sdl.cc"
                (("SDL_image.h") "SDL2/SDL_image.h"))))
          (delete 'configure)
          (replace 'check
            (lambda* (#:key tests? make-flags #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getcwd))
                ;; Fake a terminal for the test cases.
                (setenv "TERM" "xterm-256color")
                ;; Run the tests that don't require a debug build.
                (apply invoke "make" "nondebugtest"
                       (format #f "-j~d" (parallel-job-count))
                       ;; Force command line build for test cases.
                       (append make-flags '("GAME=crawl" "TILES=")))))))))
    (synopsis "Roguelike dungeon crawler game")
    (description "Dungeon Crawl Stone Soup (also known as \"Crawl\" or DCSS
for short) is a roguelike adventure through dungeons filled with dangerous
monsters in a quest to find the mystifyingly fabulous Orb of Zot.")
    (home-page "https://crawl.develz.org")
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:expat
                   license:zlib
                   license:asl2.0))))

;; The linter here claims that patch file names should start with the package
;; name. But, in this case, the patches are inherited from crawl with the
;; "crawl-" prefix instead of "crawl-tiles-".
(define-public crawl-tiles
  (package
    (inherit crawl)
    (name "crawl-tiles")
    (arguments
     (substitute-keyword-arguments
         (package-arguments crawl)
       ((#:make-flags flags)
        #~(cons*
           (string-append "PROPORTIONAL_FONT="
                          #$(this-package-input "font-dejavu")
                          "/share/fonts/truetype/DejaVuSans.ttf")
           (string-append "MONOSPACED_FONT="
                          #$(this-package-input "font-dejavu")
                          "/share/fonts/truetype/DejaVuSansMono.ttf")
           "TILES=y"
           ;; Rename the executable to allow parallel installation with crawl.
           "GAME=crawl-tiles"
           #$flags))))
    (inputs
     (modify-inputs (package-inputs crawl)
       (prepend font-dejavu freetype glu libpng sdl2 sdl2-image sdl2-mixer)))
    (native-inputs
     (modify-inputs (package-native-inputs crawl)
       (prepend pngcrush which)))
    (synopsis "Graphical roguelike dungeon crawler game")))

(define-public lugaru
  (package
    (name "lugaru")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/osslugaru/lugaru/releases"
                                  "/download/" version
                                  "/lugaru-" version ".tar.xz"))
              (sha256
               (base32 "15zgcshy22q51rm72zi6y9z7qlgnz5iw3gczjdlir4bqmxy4gspk"))
              (patches
               (search-patches "lugaru-fix-sound.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DSYSTEM_INSTALL=ON")
           #:tests? #f))                ;no test suite
    (native-inputs (list pkg-config))
    (inputs
     (list glu
           libjpeg-turbo
           libpng
           libvorbis
           openal
           sdl2
           zlib))
    (home-page "https://osslugaru.gitlab.io")
    (synopsis "Cross-platform third-person action game")
    (description "Lugaru is a third-person action game.  The main character,
Turner, is an anthropomorphic rebel bunny rabbit with impressive combat skills.
In his quest to find those responsible for slaughtering his village, he uncovers
a far-reaching conspiracy involving the corrupt leaders of the rabbit republic
and the starving wolves from a nearby den.  Turner takes it upon himself to
fight against their plot and save his fellow rabbits from slavery.")
    (license (list license:gpl2+        ; code
                   ;; assets:
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0))))

(define-public 0ad-data
  (package
    (name "0ad-data")
    (version "0.0.26-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-data.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32 "0z9dfw2hn2fyrx70866lv5464fbagdb8dip321wq10pqb22y805j"))))
    (build-system trivial-build-system)
    (native-inputs (list tar unzip xz))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
               (tar (search-input-file %build-inputs "/bin/tar"))
               (unzip (search-input-file %build-inputs "/bin/unzip"))
               (xz-path (string-append (assoc-ref %build-inputs "xz") "/bin")))
           (setenv "PATH" xz-path)
           (mkdir out)
           (invoke tar "xvf" source "-C" out "--strip=3")
           (for-each (lambda (name)
                       (let* ((dir (string-append out "/mods/" name))
                              (file (string-append dir "/" name ".zip")))
                         (invoke unzip "-o" "-d" dir file)
                         (delete-file file)))
                     '("mod" "public"))))))
    (synopsis "Data files for 0ad")
    (description "0ad-data provides the data files required by the game 0ad.")
    (home-page "https://play0ad.com")
    (properties '((hidden? . #t)))
    (license (list (license:fsdg-compatible
                    "http://tavmjong.free.fr/FONTS/ArevCopyright.txt"
                    "Similar to the license of the Bitstream Vera fonts.")
                   (license:fsdg-compatible
                    "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts")
                   license:cc-by-sa3.0
                   license:expat
                   license:gfl1.0
                   license:gpl2+
                   license:gpl3+))))

(define-public 0ad
  (package
    (name "0ad")
    (version "0.0.26-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-build.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32 "0jzfq09ispi7740c01h6yqxqv9y3zx66d217z32pfbiiwgvns71f"))))
    ;; A snippet here would cause a build failure because of timestamps
    ;; reset.  See https://bugs.gnu.org/26734.
    (inputs
     (list 0ad-data
           curl
           enet
           fmt
           freetype
           gloox
           icu4c
           libidn
           libpng
           libsodium
           libvorbis
           libxcursor
           libxml2
           miniupnpc
           mozjs-78
           openal
           sdl2
           wxwidgets
           zlib))
    (native-inputs
     (list boost
           cmake-minimal
           cxxtest
           mesa
           pkg-config
           python-2))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("config=release" "verbose=1" "-C" "build/workspaces/gcc")
       #:tests? #f                      ;tests fail currently
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundles
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file-recursively "libraries/source/spidermonkey")
             (delete-file-recursively "libraries/source/cxxtest-4.4")
             (substitute* "build/premake/premake5.lua"
               (("rootdir\\.\\.\"\\/libraries\\/source\\/cxxtest-4.4\\/bin\\/cxxtestgen\"")
                (string-append "\"" (assoc-ref inputs "cxxtest")
                               "/bin/cxxtestgen"
                               "\"")))))
         (add-after 'unpack 'fix-mozjs-compatibility
           ;; 0ad only builds fine with a specific version of mozjs
           ;; (version 78.6 for 0ad-0.0.25).
           ;; Here we change the error in case of version mismatch to a warning,
           ;; and add some minor compatibility fixes.
           (lambda _
             (substitute* "source/scriptinterface/ScriptTypes.h"
               (("#error Your compiler is trying to use")
                "#warning Your compiler is trying to use"))
             (substitute* "source/scriptinterface/ScriptContext.cpp"
               (("JS::PrepareZoneForGC\\(")
                "JS::PrepareZoneForGC(m_cx, "))))
         (replace 'configure
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (let* ((jobs (number->string (parallel-job-count)))
                    (out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad")))
               (setenv "JOBS" (string-append "-j" jobs))
               (setenv "CC" "gcc")
               (with-directory-excursion "build/workspaces"
                 (apply invoke
                        `("./update-workspaces.sh"
                          ,(string-append "--libdir=" lib)
                          ,(string-append "--datadir=" data)
                          ;; TODO: "--with-system-nvtt"
                          "--with-system-mozjs"
                          ,@(if tests? '() '("--without-tests"))))))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (chdir "binaries")
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad"))
                    (applications (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor/128x128/apps"))
                    (metainfo (string-append out "/share/metainfo"))
                    (mime (string-append out "/share/mime/application"))
                    (0ad-data (assoc-ref inputs "0ad-data")))
               ;; data
               (copy-recursively "data" data)
               (for-each (lambda (file)
                           (symlink (string-append 0ad-data "/" file)
                                    (string-append data "/" file)))
                         '("config" "mods/mod" "mods/public" "tools"))
               ;; libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "system" "\\.so$"))
               ;; binaries
               (install-file "system/pyrogenesis" bin)
               (with-directory-excursion bin
                 (symlink "pyrogenesis" "0ad"))
               ;; resources
               (with-directory-excursion "../build/resources"
                 (install-file "0ad.desktop" applications)
                 (install-file "0ad.png" hicolor)
                 (install-file "0ad.appdata.xml" metainfo)
                 (install-file "pyrogenesis.xml" mime)))))
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "system"
                 (invoke "./test"))))))))
    (home-page "https://play0ad.com")
    (synopsis "3D real-time strategy game of ancient warfare")
    (description "0 A.D. is a real-time strategy (RTS) game of ancient
warfare.  It's a historically-based war/economy game that allows players to
relive or rewrite the history of twelve ancient civilizations, each depicted
at their peak of economic growth and military prowess.

0ad needs a window manager that supports 'Extended Window Manager Hints'.")
    (license (list license:bsd-2
                   license:bsd-3
                   license:expat
                   license:gpl2+
                   license:ibmpl1.0
                   license:isc
                   license:lgpl2.1
                   license:lgpl3
                   license:mpl2.0
                   license:zlib))))

(define-public open-adventure
  (package
    (name "open-adventure")
    (version "1.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/esr/open-adventure")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lbggjmh9g4zvnzzqz0qspmc24yg25bjalm06dlvhd22vz7hrfy5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
      #:parallel-tests? #f              ;some tests fail non-deterministically
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ;no configure script
          (add-before 'build 'use-echo
            (lambda _
              (substitute* (list "tests/Makefile" "tests/tapview")
                (("/bin/echo") (which "echo")))))
          #$@(if (this-package-native-input "ruby-asciidoctor")
                 #~((add-after 'build 'build-manpage
                      (lambda _
                        ;; This target is missing a dependency
                        (substitute* "Makefile"
                          ((".adoc.6:" line)
                           (string-append line " advent.adoc")))
                        (invoke "make" ".adoc.6")))
                    ;; There is no install target.
                    (replace 'install
                      (lambda _
                        (let ((bin (string-append #$output "/bin"))
                              (man (string-append #$output "/share/man/man6")))
                          (install-file "advent" bin)
                          (install-file "advent.6" man)))))
                 #~((replace 'install
                      (lambda _
                        (let ((bin (string-append #$output "/bin")))
                          (install-file "advent" bin)))))))))
    (native-inputs
     (append
       (list asciidoc
             cppcheck
             libedit
             pkg-config
             python-pylint
             python-pyyaml
             python-wrapper)
       (if (supported-package? ruby-asciidoctor)
           (list ruby-asciidoctor)
           '())))
    (home-page "https://gitlab.com/esr/open-adventure")
    (synopsis "Colossal Cave Adventure")
    (description
     "The original Colossal Cave Adventure from 1976 was the origin of all
text adventures, dungeon-crawl (computer) games, and computer-hosted
roleplaying games.  This is a forward port of the last version released by
Crowther & Woods, its original authors, in 1995.  It has been known as
``adventure 2.5'' and ``430-point adventure''.")
    (license license:bsd-2)))

(define-public open-adventure-2.5
  (package
    (inherit open-adventure)
    (version "2.5")
    (properties `((superseded . ,open-adventure)))))

(define-public tome4
  (package
    (name "tome4")
    (version "1.7.6")
    (synopsis "Single-player, RPG roguelike game set in the world of Eyal")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://te4.org/dl/t-engine/t-engine4-src-"
                           version ".tar.bz2"))
       (sha256
        (base32 "0338lirc4jbmq29449bw8c6src8b91yn9x14w2nxr31zh00fm7cq"))
       (modules '((guix build utils)))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list glu
           libvorbis
           luajit
           openal
           premake4
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
    (arguments
     (list
      #:tests? #false                   ;no test
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "config=release")
      ;; XXX: Building in parallel occasionally causes this build failure:
      ;;   ../src/luajit2/src/host/buildvm.c:73:10: fatal error: buildvm_arch.h:
      ;;   No such file or directory
      #:parallel-build? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (replace 'configure
            (lambda _
              (invoke "premake4" "gmake")))
          (add-after 'set-paths 'set-sdl-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CPATH"
                      (string-append
                       (search-input-directory inputs "/include/SDL2")
                       ":" (or (getenv "CPATH") "")))))
          ;; premake doesn't provide install target
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (usr (string-append out "/usr"))
                     (bin (string-append out "/bin"))
                     (licenses (string-append out "/share/licenses"))
                     (documents (string-append out "/share/doc"))
                     (pixmaps (string-append out "/share/pixmaps"))
                     (icon "te4-icon.png")
                     (data (string-append out "/share/" #$name))
                     (applications (string-append
                                    out "/share/applications"))
                     (unzip (string-append
                             (assoc-ref inputs "unzip") "/bin/unzip"))
                     (wrapper (string-append bin "/" #$name)))
                ;; icon
                (mkdir-p pixmaps)
                (invoke unzip "-j"
                        (string-append
                         "game/engines/te4-" #$version ".teae")
                        (string-append
                         "data/gfx/" icon) "-d" pixmaps)
                ;; game executable
                (install-file "t-engine" data)
                (mkdir-p bin)
                (with-output-to-file wrapper
                  (lambda ()
                    (display
                     (string-append
                      "#!/bin/sh\n"
                      ;; No bootstrap code found,
                      ;; defaulting to working directory
                      ;; for engine code!
                      "cd " data "\n"
                      "exec -a tome4 ./t-engine \"$@\"\n"))))
                (chmod wrapper #o555)
                ;; licenses
                (for-each (lambda (file)
                            (install-file file licenses))
                          '("COPYING" "COPYING-MEDIA"))
                ;; documents
                (for-each (lambda (file)
                            (install-file file documents))
                          '("CONTRIBUTING" "CREDITS"))
                ;; data
                (copy-recursively "bootstrap" (string-append
                                               data "/bootstrap"))
                (copy-recursively "game" (string-append data "/game"))
                ;; launcher
                (mkdir-p applications)
                (make-desktop-entry-file
                 (string-append applications "/" #$name ".desktop")
                 #:name "ToME4"
                 #:comment #$synopsis
                 #:exec #$name
                 #:icon icon
                 #:categories '("Game" "RolePlaying"))))))))
    (home-page "https://te4.org")
    (description "Tales of Maj’Eyal (ToME) RPG, featuring tactical turn-based
combat and advanced character building.  Play as one of many unique races and
classes in the lore-filled world of Eyal, exploring random dungeons, facing
challenging battles, and developing characters with your own tailored mix of
abilities and powers.")
    (license license:gpl3+)))

(define-public torcs
  (package
    (name "torcs")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/" name
                           "/all-in-one/" version "/"
                           name "-" version ".tar.bz2"))
       ;; Source archive is in fact in gzip format, rename it.
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bx7i67b01yfy9lyak4x4xrdb3zb0mr8kwx6h8cl2dpv8lspg5jb"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (ice-9 regex)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; Delete Windows-specific sources and pre-built binaries.
            (delete-file-recursively "src/windows")
            ;; The license of the kw-* and pw-* car models includes a
            ;; non-commercial clause, hence does not comply with the GNU FSDG.
            (with-directory-excursion "data/cars/models"
              (for-each delete-file-recursively
                        (scandir "." (cut string-match "^(kc|pw)-" <>))))
            ;; Delete extraneous CVS directories.
            (for-each delete-file-recursively
                      (find-files "." (lambda (file stat)
                                        (and (eq? 'directory (stat:type stat))
                                             (string=? "CVS" (basename file))))
                                  #:directories? #t))))))
    (build-system gnu-build-system)
    (arguments
     ;; Building in parallel fails due to a race where include files have not
     ;; yet been generated, with errors such as "controlconfig.cpp:30:10:
     ;; fatal error: tgfclient.h: No such file or directory".  The issue was
     ;; reported to the 'torcs-devel' mailing list (see:
     ;; https://sourceforge.net/p/torcs/mailman/message/58834764/).
     (list #:modules `(,@%default-gnu-modules (srfi srfi-26))
           #:parallel-build? #f
           #:tests? #f                  ;no test suite
           ;; Ensure the binaries find libraries provided by this very package
           ;; (see: https://issues.guix.gnu.org/73979).
           #:configure-flags
           #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output
                                  "/lib/torcs/lib"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-commands
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/linux/torcs.in"
                     (("/bin/bash")
                      (search-input-file inputs "bin/bash")))))
               (add-after 'install 'install-data
                 (lambda _
                   (invoke "make" "datainstall")))
               (add-after 'install-data 'install-doc
                 (lambda _
                   (let ((docdir (string-append #$output "/share/doc/torcs/"))
                         (man6 (string-append #$output "/share/man/man6")))
                     (for-each (cut install-file <> man6)
                               (find-files "doc/man" "\\.6$"))
                     (install-file "doc/userman/how_to_drive.html" docdir)
                     (install-file "doc/faq/faq.html" docdir)
                     (copy-recursively "doc/userman/images"
                                       (string-append docdir "/images")))))
               (add-after 'install 'install-freedesktop-entry
                 (lambda _
                   (let ((iconsdir (string-append #$output "/share/icons/hicolor/"
                                                  "48x48/apps")))
                     (mkdir-p iconsdir)
                     (copy-file "Ticon.png" (string-append iconsdir "/torcs.png")))
                   (install-file "torcs.desktop"
                                 (string-append #$output
                                                "/share/applications/"))))
               (add-after 'install 'fix-permissions
                 ;; XXX: Otherwise, the guix daemon reports: "suspicious
                 ;; ownership or permission on /gnu/store/xxx-torcs-1.3.7',
                 ;; rejecting this build output".
                 (lambda _
                   (chmod #$output #o755))))))
    (inputs
     (list bash-minimal
           freealut
           freeglut
           libice
           libpng
           libsm
           libvorbis
           libxi
           libxmu
           libxrandr
           libxrender
           libxt
           mesa
           openal
           plib
           zlib))
    (home-page "https://sourceforge.net/projects/torcs/")
    (synopsis "Car racing simulator")
    (description "TORCS stands for The Open Racing Car Simulator.  It can be
used as an ordinary car racing game, as an artificial intelligence (AI) racing
game, or as a research platform.  The game has features such as:
@itemize
@item Input support for a driving wheel, joystick, keyboard or mouse
@item More than 30 car models
@item 30 tracks
@item 50 opponents to race against
@item Lighting, smoke, skidmarks and glowing brake disks graphics
@item Simple damage model and collisions
@item Tire and wheel properties (springs, dampers, stiffness, etc.)
@item Aerodynamics (ground effect, spoilers, etc.)
@end itemize
The difficulty level can be configured, impacting how much damage is caused by
collisions and the level of traction the car has on the track, which makes the
game fun for both novice and experts.")
    (license (list license:gpl2+        ;source and most assets
                   license:fdl1.2+))))  ;how_to_drive.html, faq.html

(define-public quakespasm
  (package
    (name "quakespasm")
    (version "0.96.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/quakespasm/Source/quakespasm-"
                           version ".tar.gz"))
       (sha256
        (base32 "0hr58w1d2yw82vm9lkln05z6d4sjlcr6grxhf6sqdqwyfy9nv1mw"))))
    (arguments
     (list #:tests? #f
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                "MP3LIB=mpg123"
                                "USE_CODEC_FLAC=1"
                                "USE_CODEC_MIKMOD=1"
                                "USE_SDL2=1"
                                "-CQuake")
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-after 'unpack 'fix-makefile-paths
                          (lambda _
                            (mkdir-p (string-append #$output "/bin"))
                            (substitute* "Quake/Makefile"
                              (("/usr/local/games")
                               (string-append #$output "/bin"))))))))
    (build-system gnu-build-system)
    (inputs (list libmikmod
                  libvorbis
                  flac
                  mesa
                  mpg123
                  sdl2))
    (synopsis "First person shooter engine for Quake 1")
    (description "Quakespasm is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "https://quakespasm.sourceforge.net/")
    (license license:gpl2+)))

(define-public vkquake
  (package
    (inherit quakespasm)
    (name "vkquake")
    (version "1.01.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Novum/vkQuake")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iwin8j5kbyrknbkhjgpy8nmm7pxqzr0daa9gn7p38qhg2mh0a39"))))
    (arguments
     `(#:make-flags
       (let ((vulkanlib (string-append (assoc-ref %build-inputs
                                                  "vulkan-loader") "/lib")))
         (list "CC=gcc"
               "MP3LIB=mpg123"
               "USE_CODEC_FLAC=1"
               "USE_CODEC_MIKMOD=1"
               "USE_SDL2=1"
               (string-append "LDFLAGS=-Wl,-rpath=" vulkanlib)
               "-CQuake"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-for-new-vulkan
                    (lambda _
                      ;; Mimic upstream commit a869a22d9b51c68e for
                      ;; compatibility with newer vulkan-headers.
                      (substitute* "Quake/gl_rmisc.c"
                        (("VK_DYNAMIC_STATE_RANGE_SIZE")
                         "3"))
                      #t))
                  (delete 'configure)
                  (add-after 'unpack 'fix-makefile-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((vulkan (assoc-ref %build-inputs
                                               "vulkan-loader"))
                            (out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (substitute* "Quake/Makefile" ((" /usr")
                                                       (string-append " " out)))
                        (substitute* "Quake/Makefile" (("/games")
                                                       (string-append "/bin")))
                        (substitute* "Quake/Makefile" (("..VULKAN_SDK.") vulkan))
                        #t))))
       ,@(strip-keyword-arguments '(#:make-flags #:phases)
                                  (package-arguments quakespasm))))
    (inputs (modify-inputs (package-inputs quakespasm)
              (prepend vulkan-headers vulkan-loader)
              (replace "sdl2" sdl2-2.0)))
    (description "vkquake is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "https://github.com/Novum/vkQuake")))

(define-public yamagi-quake2
  (package
    (name "yamagi-quake2")
    (version "8.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://deponie.yamagi.org/quake2/quake2-"
                           version ".tar.xz"))
       (sha256
        (base32 "11lv22y5ccd80iyhk6zj94wligcbx6x5vwbqh3jkgz96v0x5dng2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "CC=gcc"
             ;; An optional directory where it will look for quake2 data files
             ;; in addition to the current working directory.
             "WITH_SYSTEMWIDE=yes"
             "WITH_SYSTEMDIR=\"/opt/quake2\"")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The game writes paths to openal.so and curl.so to ~/.yq2/...
             ;; Workaround: hard-code the compiled paths where it loads them;
             ;; this prevents loading old or garbage collected libraries.
             (substitute* "src/client/sound/qal.c"
               (("al_driver->string")
                (string-append "\"" (assoc-ref inputs "openal")
                               "/lib/libopenal.so\"")))
             (substitute* "src/client/curl/qcurl.c"
               (("cl_libcurl->string")
                (string-append "\"" (assoc-ref inputs "curl")
                               "/lib/libcurl.so\"")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/bin"))
               ;; The yamagi-quake2 binary must be in the same directory
               ;; as its engine libraries, but symlinking it to /bin is okay.
               ;; https://github.com/yquake2/yquake2/blob/master/stuff/packaging.md
               (copy-recursively "release"
                                 (string-append out "/lib/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/quake2")
                        (string-append out "/bin/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/q2ded")
                        (string-append out "/bin/yamagi-q2ded"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl
           libvorbis
           mesa
           openal
           sdl2
           zlib))
    (synopsis "First person shooter engine based on quake2")
    (description "Yamagi Quake II is an enhanced client for id Software's Quake II.
The main focus is an unchanged single player experience like back in 1997,
thus the gameplay and the graphics are unaltered.  However the user may use one
of the unofficial retexturing packs.  In comparison with the official client,
over 1000 bugs were fixed and an extensive code audit done,
making Yamagi Quake II one of the most solid Quake II implementations available.")
    (home-page "https://www.yamagi.org/quake2/")
    (license (list license:gpl2+         ; game and server
                   (license:non-copyleft ; info-zip
                    "file://LICENSE"
                    "See Info-Zip section.")
                   license:public-domain)))) ; stb

(define-public nudoku
  (package
    (name "nudoku")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jubalh/nudoku")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12v00z3p0ymi8f3w4b4bgl4c76irawn3kmd147r0ap6s9ssx2q6m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal pkg-config))
    (inputs
     (list ncurses))
    (home-page "https://jubalh.github.io/nudoku/")
    (synopsis "Sudoku for your terminal")
    (description "Nudoku is a ncurses-based Sudoku game for your terminal.")
    (license license:gpl3+)))

(define-public the-butterfly-effect
  (package
    (name "the-butterfly-effect")
    (version "0.9.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/the-butterfly-effect/tbe")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ag2cp346f9bz9qy6za6q54id44d2ypvkyhvnjha14qzzapwaysj"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-cmake-install-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("/usr") (assoc-ref outputs "out"))
               (("TBE_BIN_DIR     games") "TBE_BIN_DIR     bin"))))
         (add-after 'unpack 'disable-translations
           ;; TODO: Re-enable translations when they no longer fail to build.
           (lambda _
             (substitute* "CMakeLists.txt"
               ((".*i18n.*") "")))))
       ;; Test suite requires a running Xorg server. Even when
       ;; provided, it fails with "D-Bus library appears to be
       ;; incorrectly set up; failed to read machine uuid: Failed to
       ;; open "/etc/machine-id": No such file or directory" along
       ;; with multiple "QPainter:: ... Painter not active" warnings.
       #:tests? #f))
    (inputs
     (list qtbase-5 qtsvg-5))
    (native-inputs
     `(("gettext-minimal" ,gettext-minimal)
       ("qttools-5" ,qttools-5)
       ("qtwayland-5" ,qtwayland-5)))
    (synopsis "Realistic physics puzzle game")
    (description "The Butterfly Effect (tbe) is a game that uses
realistic physics simulations to combine lots of simple mechanical
elements to achieve a simple goal in the most complex way possible.")
    (home-page "http://the-butterfly-effect.org/")
    ;; Main license is GPL2-only.  However, artwork is distributed
    ;; under various licenses, listed here.
    (license (list license:gpl2 license:public-domain license:expat
                   license:cc-by-sa3.0 license:gpl3+ license:wtfpl2))))

(define-public the-powder-toy
  (package
    (name "the-powder-toy")
    (version "98.2.365")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/The-Powder-Toy/The-Powder-Toy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06l39w3ggrzn8799dqll606by4f88kjr60r879w8j26csx1py76g"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "meson.build"
                   (("'aarch64', 'x86_64'")
                    (string-append "'aarch64', 'loongarch64', 'mips64', 'ppc64', "
                                   "'riscv64', 'sparc64', 'x86_64'")))))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags (list (string-append
                                "-Dworkaround_elusive_bzip2_include_dir="
                                (assoc-ref %build-inputs "bzip2"))
                               ,@(if (this-package-input "lua")
                                     `("-Dlua=lua-5.2"
                                       "-Dworkaround_noncpp_lua=true")
                                     '()))
       #:phases
       (modify-phases %standard-phases
         ;; Our lua variants are named lua-<version>, not lua<version>.
         (add-after 'unpack 'adjust-lua-variant-names
           (lambda _
             (substitute* '("meson.build"
                            "meson_options.txt")
               (("lua5\\.") "lua-5."))))
         ;; No install target
         (replace 'install
           (lambda _
             (let* ((output (assoc-ref %outputs "out"))
                    (bin (string-append output "/bin"))
                    (share (string-append output "/share"))
                    (apps (string-append share "/applications")))
               (mkdir-p bin)
               (mkdir-p apps)
               (install-file "powder" bin)
               (install-file "resources/powder.desktop" apps)
               (for-each
                 (lambda (size)
                   (let ((dir (string-append share "/icons/hicolor/"
                                             size "x" size "/apps")))
                     (mkdir-p dir)
                     (copy-file (string-append (assoc-ref %build-inputs "source")
                                               "/resources/generated_icons/icon_exe_"
                                               size ".png")
                                (string-append dir "/powdertoy-powder.png"))))
                 '("16" "32" "48"))))))))
    (native-inputs (list pkg-config))
    (inputs
     (append
       (list bzip2
             curl
             fftwf
             jsoncpp
             libpng)
       (if (supported-package? luajit)
           (list luajit)
           (list lua-5.2))
       (list sdl2
             zlib)))
    (synopsis "Free physics sandbox game")
    (description
     "The Powder Toy is a free physics sandbox game, which simulates air
pressure and velocity, heat, gravity and a countless number of interactions
between different substances!  The game provides you with various building
materials, liquids, gases and electronic components which can be used to
construct complex machines, guns, bombs, realistic terrains and almost
anything else.  You can then mine them and watch cool explosions, add
intricate wirings, play with little stickmen or operate your machine.")
    (home-page "https://powdertoy.co.uk/")
    ;; The few files that have a license header specify GPLv3+, but most don't
    ;; and it's not otherwise specified anywhere.
    (license license:gpl3)))

(define-public pioneer
  (package
    (name "pioneer")
    (version "20220203")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pioneerspacesim/pioneer")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qjq6lsr1rmcnvq9b7r745cpp7n0q6cpc3k81q8ai4xspbq61m8w"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list assimp
           curl
           freetype
           glew
           glu
           libpng
           libsigc++-2
           libvorbis
           lua-5.2                      ;not compatible with 5.3
           mesa
           (sdl-union (list sdl2 sdl2-image))))
    (arguments
     `(#:tests? #f                      ;tests are broken
       #:configure-flags (list "-DUSE_SYSTEM_LIBLUA:BOOL=YES"
                               "-DUSE_SYSTEM_LIBGLEW:BOOL=YES"
                               (string-append "-DPIONEER_DATA_DIR="
                                              %output "/share/games/pioneer"))
       #:make-flags (list "all" "build-data")))
    (home-page "https://pioneerspacesim.net")
    (synopsis "Game of lonely space adventure")
    (description
     "Pioneer is a space adventure game set in our galaxy at the turn of the
31st century.  The game is open-ended, and you are free to eke out whatever
kind of space-faring existence you can think of.  Look for fame or fortune by
exploring the millions of star systems.  Turn to a life of crime as a pirate,
smuggler or bounty hunter.  Forge and break alliances with the various
factions fighting for power, freedom or self-determination.  The universe is
whatever you make of it.")
    (license license:gpl3)))

(define-public badass
  (package
    (name "badass")
    (version "0.0.0-20151201180210-02e7c38d503f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/umayr/badass")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b0y7xz9y0cqxfphfznz2zhfk7mm2vvmldf088w2fm8c8spj4g28"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/umayr/badass"))
    (home-page "https://github.com/umayr/badass")
    (synopsis "Hacking contribution graphs in git")
    (description
     "Badass generates false commits for a range of dates, essentially
hacking the gamification of contribution graphs on platforms such as
Github or Gitlab.")
    (license license:expat)))

(define-public colobot
  (package
    (name "colobot")
    (version "0.2.1-alpha")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/colobot/colobot")
             (commit (string-append "colobot-gold-" version))
             (recursive? #t)))          ;for "data/" subdir
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bpy5nzkvq5nfr0w8jf7bl7zs8yz2cpzp87pnkdlgwl3adcn9nsw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match)
                  (ice-9 regex))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-git-checkout-writable
            (lambda _
              (for-each make-file-writable (find-files "."))))
          (add-after 'unpack 'fix-directories
            (lambda _
              (substitute* "CMakeLists.txt"
                (("(\\$\\{CMAKE_INSTALL_PREFIX\\})/games" _ prefix)
                 (string-append prefix "/bin"))
                (("(\\$\\{CMAKE_INSTALL_PREFIX\\}/share)/games/colobot" _ prefix)
                 (string-append prefix "/colobot")))))
          (add-after 'fix-directories 'install-music
            ;; Retrieve and install music files.
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Installation process tries to download music files using
              ;; "wget" if not already present.  Since we are going another
              ;; route, skip "wget" command check.
              (substitute* "data/music/CMakeLists.txt"
                (("find_program\\(WGET wget\\)") ""))
              ;; Populate "music/" directory.
              (let ((data
                     (any
                      (match-lambda ((_ . input)
                                     (and (string-match "colobot-music" input)
                                          input)))
                      inputs)))
                (invoke "tar" "-xvf" data "-Cdata/music")))))))
    (native-inputs
     (list (origin
             (method url-fetch)
             (uri (string-append "https://colobot.info/files/music/"
                                 "colobot-music_ogg_" version ".tar.gz"))
             (sha256
              (base32
               "1s86cd36rwkff329mb1ay1wi5qqyi35564ppgr3f4qqz9wj9vs2m")))
           gettext-minimal
           (librsvg-for-system)
           po4a
           python-wrapper))
    (inputs
     (list boost
           glew
           libogg
           libpng
           libsndfile
           libvorbis
           openal
           physfs
           (sdl-union (list sdl2 sdl2-image sdl2-ttf))))
    (synopsis "Educational programming strategy game")
    (description "Colobot: Gold Edition is a real-time strategy game, where
you can program your units (bots) in a language called CBOT, which is similar
to C++ and Java.  Your mission is to find a new planet to live and survive.
You can save humanity and get programming skills!")
    (home-page "https://colobot.info")
    (license license:gpl3+)))

(define-public gzdoom
  (package
    (name "gzdoom")
    (version "4.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coelckers/gzdoom")
             (commit (string-append "g" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i4hyg72z84fc6ca2ic9q82q5cbgrbd7bynl3kpkypxvyasq08wz"))
       (patches (search-patches "gzdoom-search-in-installed-share.patch"
                                "gzdoom-find-system-libgme.patch"))
       (modules '((guix build utils)
                  (ice-9 regex)))
       (snippet
        '(begin
           ;; Remove files which mustn't be commercially redistributed.  See
           ;; <https://zdoom.org/wiki/License#Commercial_use>, the ‘Contribution
           ;; Guidelines’ at <https://github.com/ZDoom>, and Guix issue #73435.
           (for-each
            (lambda (directory)
              (delete-file-recursively directory)
              (substitute* "CMakeLists.txt"
                (((string-append "add_subdirectory\\([[:blank:]]*"
                                 directory
                                 "[[:blank:]]*\\)"))
                 "")))
            '( ;; "wadsrc_extra"        ;game_support.pk3
              "wadsrc_bm"))             ;brightmaps.pk3

           ;; Removing game_support.pk3 entirely would break Freedoom & remove
           ;; users' ability to play commercial games, despite owning (only) the
           ;; non-functional data.  That can't be right.  Out of an abundance of
           ;; caution, remove anything from the PK3 that could conceivably be
           ;; derived from copyrightable data that's not freely redistributable.
           (display "Keeping only the following game_support.pk3 files:\n")
           (let* ((regexps (list "/font\\.inf$"
                                 "/harmony/.*\\.(txt|zs)$"
                                 "/(iwadinfo|mapinfo|sprofs)\\.txt$"
                                 "\\.z$"))
                  (regexp* (format #f "(~{~a~^|~})" regexps))
                  (regexp  (make-regexp regexp* regexp/icase)))
             (define (keep-file? file stat)
               (let ((keep? (regexp-exec regexp file)))
                 (when keep?
                   (format #t "  ~a~%" file))
                 keep?))

             (for-each delete-file (find-files "wadsrc_extra/static"
                                               (negate keep-file?))))

           ;; Remove some bundled libraries.  XXX There are more, but removing
           ;; them would require, at least, patching the build system.
           (with-directory-excursion "libraries"
             (delete-file-recursively "bzip2")
             (delete-file-recursively "game-music-emu")
             (delete-file-recursively "jpeg")
             (delete-file-recursively "zlib"))))))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
         (string-append
          "-DCMAKE_CXX_FLAGS:="
          "-DSHARE_DIR=\\\"" #$output "/share/\\\" "
          "-DGUIX_OUT_PK3=\\\"" #$output "/share/games/doom\\\"")

         ;; The build requires some extra convincing not to use the bundled
         ;; libgme previously deleted in the soure snippet.
         "-DFORCE_INTERNAL_GME=OFF"

         ;; Link libraries at build time instead of loading them at run time.
         "-DDYN_OPENAL=OFF"
         "-DDYN_FLUIDSYNTH=OFF"
         "-DDYN_GTK=OFF"
         "-DDYN_MPG123=OFF"
         "-DDYN_SNDFILE=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/CMakeLists.txt"
                (("COMMAND /bin/sh")
                 (string-append "COMMAND " (which "sh"))))
              (substitute*
                  "libraries/zmusic/mididevices/music_fluidsynth_mididevice.cpp"
                (("/usr/share/sounds/sf2/FluidR3_GM.sf2")
                 (search-input-file inputs
                                    "share/soundfonts/FluidR3Mono_GM.sf3")))
              (substitute*
                  "libraries/zmusic/mididevices/music_timiditypp_mididevice.cpp"
                (("(exename = \")(timidity)(\".*)" _ prefix exe suffix)
                 (string-append prefix
                                (search-input-file inputs
                                                   (string-append "bin/" exe))
                                suffix))))))))
    (build-system cmake-build-system)
    (inputs (list bzip2
                  fluid-3
                  fluidsynth
                  gtk+
                  libgme
                  libjpeg-turbo
                  libsndfile
                  mesa
                  mpg123
                  openal
                  sdl2
                  timidity++
                  zlib))
    (native-inputs (list pkg-config unzip))
    (synopsis "Modern Doom 2 source port")
    (description "GZdoom is a port of the Doom 2 game engine, with a modern
renderer.  It improves modding support with ZDoom's advanced mapping features
and the new ZScript language.  In addition to Doom, it supports Heretic, Hexen,
Strife, Chex Quest, and fan-created games like Harmony, Hacx and Freedoom.")
    (home-page "https://zdoom.org/index")
    ;; The source uses x86 assembly
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license (list license:gpl3+         ; gzdoom game
                   license:lgpl3+        ; gzdoom renderer
                   license:expat         ; gdtoa
                   (license:non-copyleft ; modified dumb
                    "file://dumb/licence.txt"
                    "Dumb license, explicitly GPL compatible.")))))

(define-public odamex
  (package
    (name "odamex")
    (version "10.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/odamex/Odamex/" version "/"
             "odamex-src-" version ".tar.xz"))
       (sha256
        (base32 "1vy6d0md5ws5319bjjbaqnca68vslgk22k9lh4yd9n85hzlwacpi"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; XXX: Unbundle more, they are not replaced by the ones provided
           ;; in inputs: fltk, jsoncpp, miniupnp, protobuf.
           ;;
           ;; Remove some bundled libraries.
           (with-directory-excursion "libraries"
             (for-each delete-file-recursively
                       '("curl" "libpng" "portmidi" "zlib")))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:configure-flags
      #~(list "-DBUILD_CLIENT=1"
              "-DBUILD_MASTER=1"
              "-DBUILD_SERVER=1"
              "-DUSE_INTERNAL_LIBS=0"
              "-DUSE_INTERNAL_MINIUPNP=0")))
    (native-inputs
     (list deutex pkg-config))
    (inputs
     (list alsa-lib
           curl
           fltk
           jsoncpp
           libpng
           miniupnpc
           portmidi
           protobuf
           sdl2
           sdl2-mixer
           zlib))
    (home-page "https://odamex.net/")
    (synopsis "Multiplayer Doom port")
    (description "Odamex is a modification of the Doom engine that
allows players to easily join servers dedicated to playing Doom
online.")
    (license license:gpl2+)))

(define-public chocolate-doom
  (package
    (name "chocolate-doom")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.chocolate-doom.org/downloads/"
                                  version
                                  "/chocolate-doom-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "1iy8rx7kjvi1zjiw4zh77szzmd1sgpqajvbhprh1sj93fhbxcdfl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "CFLAGS=-fcommon")))
    (inputs (list sdl2-net sdl2-mixer sdl2))
    (native-inputs
     (list pkg-config))
    (synopsis "Doom source port preserving the look, feel, and bugs of vanilla
Doom")
    (description
     "Chocolate Doom takes a different approach to other source ports.  Its
aim is to accurately reproduce the experience of playing Vanilla Doom.  It is
a conservative, historically accurate Doom source port, which is compatible
with the thousands of mods and levels that were made before the Doom source
code was released.  Rather than flashy new graphics, Chocolate Doom's main
features are its accurate reproduction of the game as it was played in the
1990s.  The project is developed around a carefully-considered philosophy that
intentionally restricts which features may be added (and rejects any that
affect gameplay).")
    (home-page "https://www.chocolate-doom.org/wiki/index.php/Chocolate_Doom")
    (license license:gpl2)))

(define-public crispy-doom
  (package
    (inherit chocolate-doom)
    (name "crispy-doom")
    (version "5.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fabiangreffrath/crispy-doom")
                    (commit (string-append "crispy-doom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1b6gn0dysv631jynh769whww9xcss1gms78sz3nrn855q1dsvcb4"))))
    (native-inputs
     (append
      (package-native-inputs chocolate-doom)
      `(("automake" ,automake)
        ("autoreconf" ,autoconf))))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; The bundled autogen.sh script unconditionally runs ./configure.
           (lambda _ (invoke "autoreconf" "-vif"))))))
    (synopsis "Limit-removing enhanced-resolution Doom source port based on
Chocolate Doom")
    (description
     "Crispy Doom is a friendly fork of Chocolate Doom that provides a higher
display resolution, removes the static limits of the Doom engine and offers
further optional visual, tactical and physical enhancements while remaining
entirely config file, savegame, netplay and demo compatible with the
original.")
    (home-page "https://www.chocolate-doom.org/wiki/index.php/Crispy_Doom")))

(define-public woof-doom
  (package
    (name "woof-doom")
    (version "15.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fabiangreffrath/woof")
             (commit (string-append "woof_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04c7hm4jnr9aiz6w4520zww6b7j86qv9xaf87hdv48cjc9sp2ljk"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (with-directory-excursion "third-party"
                     (delete-file-recursively "miniz")
                     (delete-file-recursively "yyjson")
                     (delete-file-recursively "spng"))
                   (delete-file-recursively "win32")
                   (substitute* (find-files "src" ".")
                     (("miniz.h") "miniz/miniz.h"))))
       (patches (search-patches "woof-doom-unbundle-spng-miniz.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;'demotest' requires internet access.
    (native-inputs (list python))
    (inputs (list libebur128
                  libsndfile
                  libxmp
                  miniz
                  openal
                  sdl2
                  sdl2-net
                  spng
                  yyjson
                  fluidsynth))
    (home-page "https://github.com/fabiangreffrath/woof")
    (synopsis "MBF-style Doom source port targeted at modern systems")
    (description
     "Woof! is a continuation of the MBF lineage of Doom source ports, with
modern features such as dynamic resolution scaling, uncapped framerates,
adjustable field of view, 3D audio with HRTF and 7.1 surround sound
support, and modern gamepad features including rumble, gyro, and flick
stick support.  Supports the new MBF21 format, as well as the MUSINFO,
UMAPINFO, DEHEXTRA, and DSDHacked specifictions.")
    (license
     ;; See README.md
     (list (license:non-copyleft
            "https://bitbucket.org/jpommier/pffft/src/master/pffft.h"
            "FFTPACK license")
           license:bsd-2
           license:bsd-3
           license:cc-by3.0
           license:cc0
           license:expat
           license:gpl2
           license:gpl3+
           license:public-domain
           license:gpl2+))))

(define xonotic-data
  (package
    (name "xonotic-data")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.xonotic.org/xonotic-"
                           version ".zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "0kcnps65k81sm56s5dclahvllilnir3ix9kf5xr9jx0fh26hz1ah"))))
    (build-system trivial-build-system)
    (native-inputs (list unzip))
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((out (assoc-ref %outputs "out"))
                      (xonotic (string-append out "/share/xonotic"))
                      (source (assoc-ref %build-inputs "source"))
                      (unzip (search-input-file %build-inputs "/bin/unzip")))
                 (invoke unzip source)
                 (chdir "Xonotic")
                 (install-file "key_0.d0pk" xonotic)
                 (copy-recursively "data"
                                   (string-append xonotic "/data"))
                 (copy-recursively "server"
                                   (string-append xonotic "/server"))))))
    (home-page "https://xonotic.org")
    (synopsis "Data files for Xonotic")
    (description
     "Xonotic-data provides the data files required by the game Xonotic.")
    (license (list license:gpl2+
                   (license:x11-style "file://server/rcon.pl")))))

(define-public xonotic
  (package
    (name "xonotic")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.xonotic.org/xonotic-"
                           version "-source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1a0j825rb86i34xc5k6spsma41gcgp6yl8qs2affhjpz3iwar4lb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--prefix=" #$output)
                   "--disable-rijndael")
           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-26))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'build-darkplaces
                 (lambda* (#:key make-flags parallel-build? outputs
                           #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (share (string-append out "/share/xonotic/")))
                     (apply invoke "make"
                            "-C" "source/darkplaces"
                            "-f" "makefile"
                            "-j" (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")
                            (string-append "CC=" #$(cc-for-target))
                            (string-append "DP_FS_BASEDIR=" share)
                            "DP_LINK_TO_LIBJPEG=1"
                            "DP_SOUND_API=ALSA"
                            "cl-release"
                            "sdl-release"
                            "sv-release"
                            make-flags))))
               (add-before 'configure 'preconfigure
                 (lambda _
                   (chdir "source/d0_blind_id")
                   (invoke "sh" "autogen.sh")))
               (add-after 'install 'symlink-data
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (data (assoc-ref inputs "xonotic-data")))
                     (symlink (string-append data "/share/xonotic")
                              (string-append out "/share/xonotic")))))
               (add-after 'install 'install-desktop-entries
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (app (string-append out "/share/applications")))
                     ;; Add .desktop files for the 2 variants and the symlink.
                     (for-each
                      (lambda (variant)
                        (let* ((file (if variant
                                         (format #f "xonotic-~(~a~)" variant)
                                         "xonotic"))
                               (name (if variant
                                         (format #f "Xonotic (~a)" variant)
                                         "Xonotic"))
                               (exec (string-append out "/bin/" file)))
                          (make-desktop-entry-file
                           (string-append app "/" file ".desktop")
                           #:name name
                           #:comment `((#f #$(package-synopsis this-package)))
                           #:exec exec
                           #:try-exec exec
                           #:icon "xonotic"
                           #:categories '("Game"))))
                      (list #f "GLX" "SDL")))))
               (add-after 'install 'install-icons
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (for-each
                      (lambda (file)
                        (let* ((size (string-filter char-numeric? file))
                               (icons (string-append out "/share/icons/hicolor/"
                                                     size "x" size "/apps")))
                          (mkdir-p icons)
                          (copy-file file (string-append icons "/xonotic.png"))))
                      (find-files "../../misc/logos/icons_png"
                                  "^xonotic_[0-9]+\\.png$")))))
               (add-after 'install 'install-binaries
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (for-each
                      (lambda (variant)
                        (copy-file
                         (string-append "../darkplaces/darkplaces-" variant)
                         (string-append bin "/xonotic-" variant)))
                      (list "dedicated" "glx" "sdl")))))
               (add-after 'install-binaries 'wrap-binaries
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   ;; All games must be wrapped to get sound and networking.
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (curl (assoc-ref inputs "curl"))
                          (vorbis (assoc-ref inputs "libvorbis")))
                     (for-each (cut wrap-program <>
                                    `("LD_LIBRARY_PATH" ":" prefix
                                      (,(string-append curl "/lib:"
                                                       vorbis "/lib"))))
                               (find-files bin "^xonotic"))

                     ;; Provide a default xonotic executable, defaulting to SDL.
                     (symlink "xonotic-sdl" (string-append bin "/xonotic"))))))))
    (native-inputs
     (list autoconf
           automake
           gmp
           libtool
           pkg-config
           unzip))
    (inputs
     (list alsa-lib
           bash-minimal
           curl
           libjpeg-turbo
           libmodplug
           libvorbis
           libogg
           libpng
           libx11
           libxpm
           libxxf86dga
           libxxf86vm
           libxext
           libxau
           libxdmcp
           mesa
           glu
           freetype
           sdl2
           hicolor-icon-theme
           xonotic-data))
    (home-page "https://xonotic.org")
    (synopsis "Fast-paced first-person shooter game")
    (description
     "Xonotic is a free, fast-paced first-person shooter.
The project is geared towards providing addictive arena shooter
gameplay which is all spawned and driven by the community itself.
Xonotic is a direct successor of the Nexuiz project with years of
development between them, and it aims to become the best possible
open-source FPS of its kind.")
    (license (list license:gpl2+
                   license:bsd-3 ; /source/d0_blind_id folder and others
                   (license:x11-style "" "See file rcon.pl.")))))

(define-public frotz
  (package
    (name "frotz")
    (version "2.54")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1vsfq9ryyb4nvzxpnnn40k423k9pdy8k67i8390qz5h0vmxw0fds"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; there are no tests
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure-script
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (man (string-append out "/share/man/man6")))
                     (install-file "frotz" bin)
                     (mkdir-p man)
                     (install-file "doc/frotz.6" man)))))))
    (native-inputs (list pkg-config which))
    (inputs (list ao libmodplug libsamplerate libsndfile libvorbis ncurses))
    (synopsis "Portable Z-machine interpreter (ncurses version) for text adventure games")
    (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
complies with standard 1.0 of Graham Nelson's specification.  It plays all
Z-code games V1-V8, including V6, with sound support through libao, and uses
ncurses for text display.")
    (home-page "https://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public naev
  (package
    (name "naev")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/naev/naev")
             (commit (string-append "v" version))
             (recursive? #t))) ; for game data
       (file-name (git-file-name name version))
       (sha256
        (base32 "14rvwacvc2gqyh193w8ymaznqrrymbznndfp6f5fjcs90iqnc4p5"))))
    (build-system meson-build-system)
    (arguments
     ;; XXX: Do not add debugging symbols, which cause the build to fail.
     `(#:configure-flags (list "--buildtype=release")
       #:tests? #f))          ;sole test fails with a missing "/dev/dri" error
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list enet
           freetype
           glpk
           libpng
           libunibreak
           libvorbis
           libwebp
           libxml2
           luajit
           openal
           openblas
           pcre2
           physfs
           python
           python-pyyaml
           sdl2
           sdl2-image
           suitesparse))
    (home-page "https://naev.org/")
    (synopsis "Game about space exploration, trade and combat")
    (description
     "Naev is a 2d action/rpg space game that combines elements from
the action, RPG and simulation genres.  You pilot a spaceship from
a top-down perspective, and are more or less free to do what you want.
As the genre name implies, you’re able to trade and engage in combat
at will.  Beyond that, there’s an ever-growing number of story-line
missions, equipment, and ships; even the galaxy itself grows larger
with each release.  For the literacy-inclined, there are large amounts
of lore accompanying everything from planets to equipment.")
    (license (list license:gpl3
                   license:public-domain
                   license:expat        ;edtaa3func.c
                   license:bsd-2        ;distance_field.c
                   license:bsd-3))))    ;perlin.c

(define-public frotz-dumb-terminal
  (package
    (name "frotz-dumb-terminal")
    (version "2.44")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1v735xr3blznac8fnwa27s1vhllx4jpz7kw7qdw1bsfj6kq21v3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make" "dumb")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "dfrotz" bin)
               (mkdir-p man)
               (install-file "doc/dfrotz.6" man)
               #t))))))
    (synopsis "Portable Z-machine dumb interpreter for text adventure games")
    (description "Frotz is an interpreter for Infocom games and
other Z-machine games in the text adventure/interactive fiction genre.
dfrotz is the dumb interface version.  You get no screen control; everything
is just printed to the terminal line by line.  The terminal handles all the
scrolling.  Maybe you'd like to experience what it's like to play Adventure on
a teletype.  A much cooler use for compiling Frotz with the dumb interface is
that it can be wrapped in CGI scripting, PHP, and the like to allow people
to play games on webpages.  It can also be made into a chat bot.")
    (home-page "https://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public frotz-sdl
  (let* ((commit "4de8c34f2116fff554af6216c30ec9d41bf50b24"))
    (package
      (name "frotz-sdl")
      (version "2.45pre")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/DavidGriffith/frotz")
                      (commit commit)))
                (sha256
                 (base32
                  "18ms21pcrl7ipcnyqnf8janamkryzx78frsgd9kfk67jvbj0z2k8"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'patch-makefile
             (lambda _
               (substitute* "Makefile"
                 (("lcurses") "lncurses")
                 (("^BUILD_DATE_TIME =.*$")
                  "BUILD_DATE_TIME = \"2.45pre-20180907.00000\"\n"))
               #t))
           (replace 'build
             (lambda _
               (invoke "make" "sdl")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (man (string-append out "/share/man/man6")))
                 (install-file "sfrotz" bin)
                 (mkdir-p man)
                 (install-file "doc/sfrotz.6" man)
                 #t))))))
      (native-inputs
       (list pkg-config which perl))
      (inputs (list sdl2
                    sdl2-mixer
                    libmodplug
                    libsamplerate
                    libsndfile
                    libvorbis
                    ncurses
                    freetype
                    libjpeg-turbo))
      (synopsis "Portable Z-machine interpreter (SDL port) for text adventure games")
      (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
using SDL fully supports all these versions of the Z-Machine including the
graphical version 6.  Graphics and sound are created through the use of the SDL
libraries.  AIFF sound effects and music in MOD and OGG formats are supported
when packaged in Blorb container files or optionally from individual files.")
      (home-page "https://frotz.sourceforge.net")
      (license license:gpl2+))))

(define-public frozen-bubble
  ;; Last official release is very outdated (2010).  Use latest commit (2017).
  (let ((commit "d6a029110ad6ab9e4960052e175addc98807fb7e")
        (revision "1"))
    (package
      (name "frozen-bubble")
      (version (git-version "2.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kthakore/frozen-bubble")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rfrcym5lf4qac2qdklikb1ywijyxypq298azzxahy461dadl6cx"))))
      (build-system perl-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Build process needs to create files in the "server"
           ;; directory.
           (add-after 'unpack 'fix-permissions
             (lambda _
               (for-each make-file-writable
                         (find-files "server" "." #:directories? #t))))
           ;; By default, build stops at warnings.
           (add-after 'unpack 'prevent-build-error
             (lambda _
               (substitute* "inc/My/Builder.pm"
                 (("-Werror") ""))))
           (add-after 'install 'install-desktop-file-and-icons
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                      (hicolor (string-append share "/icons/hicolor")))
                 ;; Create desktop entry.
                 (make-desktop-entry-file
                  (string-append share "/applications/" ,name ".desktop")
                  #:name "Frozen Bubble"
                  #:comment "Frozen Bubble arcade game"
                  #:exec ,name
                  #:icon ,name
                  #:categories '("Game" "ArcadeGame"))
                 ;; Add icons.
                 (with-directory-excursion "share/icons"
                   (for-each
                    (lambda (size)
                      (let* ((dim (string-append size "x" size))
                             (dir (string-append hicolor "/" dim "/apps")))
                        (mkdir-p dir)
                        (copy-file
                         (string-append "frozen-bubble-icon-" dim ".png")
                         (string-append dir "/frozen-bubble.png"))))
                    '("16" "32" "48" "64"))))))
           (add-after 'install 'wrap-perl-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (perl5lib (getenv "PERL5LIB")))
                 (for-each (lambda (prog)
                             (wrap-program (string-append out "/" prog)
                               `("PERL5LIB" ":" prefix
                                 (,(string-append perl5lib ":" out
                                                  "/lib/perl5/site_perl")))))
                           (find-files "bin" "."))))))))
      (native-inputs
       (list perl-alien-sdl perl-capture-tiny perl-locale-maketext-lexicon
             perl-module-build pkg-config))
      (inputs
       (list bash-minimal
             glib
             perl-compress-bzip2
             perl-file-sharedir
             perl-file-slurp
             perl-file-which
             perl-ipc-system-simple
             perl-sdl
             (sdl-union (list sdl sdl-image sdl-mixer sdl-pango sdl-ttf))))
      (home-page "http://frozen-bubble.org/")
      (synopsis "Puzzle with bubbles")
      (description
       "Frozen-Bubble is a clone of the popular Puzzle Bobble game, in which
you attempt to shoot bubbles into groups of the same color to cause them to
pop.

Players compete as penguins and must use the arrow keys to aim a colored
bubble at groups of bubbles.  The objective is to clear all the bubbles off
the screen before a bubble passes below a line at the bottom.

It features 100 single-player levels, a two-player mode, music and striking
graphics.  A level editor is also included to allow players to create and play
their own levels.")
      (license license:gpl2))))

(define-public libmanette
  (package
    (name "libmanette")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libmanette/"
                                  (version-major+minor version) "/"
                                  "libmanette-" version ".tar.xz"))
              (sha256
               (base32
                "13v85gckp937lppjqk42wvkd9pafszigyr7wcm6afq1g8pjnndi9"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-resources
           gobject-introspection pkg-config vala))
    (propagated-inputs
     (list glib libevdev libgudev))     ; as per manette-0.2.pc
    (home-page "https://wiki.gnome.org/Apps/Games")
    (synopsis "Game controller library")
    (description "Libmanette is a small GObject library giving you simple
access to game controllers.  It supports the de-facto standard gamepads as
defined by the W3C standard Gamepad specification or as implemented by the SDL
GameController.")
    (license license:lgpl2.1+)))

(define-public quadrapassel
  (package
    (name "quadrapassel")
    (version "40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/quadrapassel/"
                                  (version-major version) "/"
                                  "quadrapassel-" version ".tar.xz"))
              (sha256
               (base32
                "02n0khwy38pykw4xqpnkym6xvj2sv8izfjbaxlik3iq7890j5n0b"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list desktop-file-utils           ; for desktop-file-validate
           gettext-minimal
           (list glib "bin")            ; for glib-compile-resources
           itstool
           libxml2                      ; for xmllint
           pkg-config
           vala))
    (inputs
     (list clutter
           clutter-gtk
           gsound
           gtk+
           libcanberra
           libmanette
           librsvg))
    (home-page "https://wiki.gnome.org/Apps/Quadrapassel")
    (synopsis "GNOME version of Tetris")
    (description "Quadrapassel comes from the classic falling-block game,
Tetris.  The goal of the game is to create complete horizontal lines of
blocks, which will disappear.  The blocks come in seven different shapes made
from four blocks each: one straight, two L-shaped, one square, and two
S-shaped.  The blocks fall from the top center of the screen in a random
order.  You rotate the blocks and move them across the screen to drop them in
complete lines.  You score by dropping blocks fast and completing lines.  As
your score gets higher, you level up and the blocks fall faster.")
    (license license:gpl2+)))

(define-public endless-sky
  (package
    (name "endless-sky")
    (version "0.10.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/endless-sky/endless-sky")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nwim56ii3z6f9gxvmf9q4i5chlsgk3kjisz8li6ivr595wq5502"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DES_USE_VCPKG=0"
                                     "-DES_USE_SYSTEM_LIBRARIES=1")
           #:make-flags #~(list (string-append "PREFIX=" #$output))
           #:build-type "Release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-paths
                 (lambda _
                   ;; Look for resources in the store directory.
                   (substitute* "source/Files.cpp"
                     (("/usr/local") #$output))
                   ;; Install game binary into %out/bin.
                   (substitute* "CMakeLists.txt"
                     (("games\\)") "bin)")))))))
    (inputs
     (list catch2-3
           glew
           libjpeg-turbo
           libmad
           libpng
           openal
           sdl2
           `(,util-linux "lib"))) ; for libuuid
    (home-page "https://endless-sky.github.io/")
    (synopsis "2D space trading and combat game")
    (description "Endless Sky is a 2D space trading and combat game.  Explore
other star systems.  Earn money by trading, carrying passengers, or completing
missions.  Use your earnings to buy a better ship or to upgrade the weapons and
engines on your current one.  Blow up pirates.  Take sides in a civil war.  Or
leave human space behind and hope to find friendly aliens whose culture is more
civilized than your own.")
    (license (list license:gpl3+
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:public-domain))))

(define-public speed-dreams-data
  ;; Use the commit corresponding to the 'speed-dreams-data' submodule
  ;; (https://forge.a-lec.org/speed-dreams/speed-dreams-data).
  (hidden-package
   (package
     (name "speed-dreams-data")
     (version "2.4.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url (string-append "https://forge.a-lec.org/speed-dreams/"
                                  name))
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ki620pq5gcn10l5328qsh6jdjsgrvyb4fhvgi0s9fvflzzg6905"))))
     (build-system cmake-build-system)
     (arguments (list #:tests? #f))   ;no test suite
     (home-page "https://www.speed-dreams.net/en")
     (synopsis "Data for the Speed Dreams racing game")
     (description "This package contains the non-functional data for the
Speed Dreams racing game.")
     (license license:gpl2+))))

(define-public speed-dreams
  (package
    (name "speed-dreams")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://forge.a-lec.org/"
                                 name "/" name "-code.git"))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "150mwjdv9pmc3cjchfbkprnlbsnw2gv57350lir5vbh77xrgpn8c"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:build-type "Release"
      #:configure-flags
      #~(list "-DOPTION_3RDPARTY_EXPAT=ON" ;use system expat library
              "-DSD_BINDIR:PATH=bin"       ;install to /bin instead of /games
              (string-append "-DVERSION_LONG=" #$version))))
    (native-inputs (list pkg-config speed-dreams-data))
    (inputs
     (list cjson
           curl
           enet
           expat
           freeglut
           freesolid
           freetype
           glm
           libjpeg-turbo
           libogg
           libpng
           libvorbis
           minizip
           openal
           openscenegraph
           plib
           rhash
           sdl2
           sdl2-mixer
           tinygltf
           zlib))
    (home-page "https://sourceforge.net/projects/speed-dreams/")
    (synopsis "Car racing simulator")
    (description "Speed Dreams is a car racing simulator featuring
high-quality 3D graphics and an accurate physics engine, aiming for maximum
realism.  Initially forked from TORCS, it features improvements to the
graphics and physics simulation, and supports modern input methods such as
gamepads by use of the SDL library.  It features more than 20 tracks and more
than 80 cars to race with.  Extra (freely licensed) assets can be downloaded
via the in-game download manager.")
    (license (list license:gpl2+))))

(define-public stepmania
  (package
    (name "stepmania")
    (version "5.1.0-b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stepmania/stepmania")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a7y9l7xm510vgnpmj1is7p9m6d6yd0fcaxrjcickz295k5w3rdn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove song files, which are licensed under a non-commercial
           ;; clause, and a course pointing to them.
           (for-each delete-file-recursively
                     '("Songs/StepMania 5/Goin' Under"
                       "Songs/StepMania 5/MechaTribe Assault"
                       "Songs/StepMania 5/Springtime"))
           (for-each delete-file '("Courses/Default/Jupiter.crs"
                                   "Courses/Default/Jupiter.png"))
           ;; Unbundle libpng.
           (substitute* "extern/CMakeLists.txt"
             (("include\\(CMakeProject-png.cmake\\)") ""))
           (delete-file-recursively "extern/libpng")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;FIXME: couldn't find how to run tests
       #:build-type "Release"
       #:out-of-source? #f              ;for the 'install-desktop' phase
       #:configure-flags
       (list "-DWITH_SYSTEM_FFMPEG=1"
             ;; SSE instructions are available on Intel systems only.
             ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                    (%current-system)))
                        '("x86_64" "i686"))
                   '()
                   '("-DWITH_SSE2=NO"))
             ;; Configuration cannot find GTK2 without the two following
             ;; flags.
             (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "gtk+")
                            "/lib/gtk-2.0/include")
             (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "glib")
                            "/lib/glib-2.0/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ensure-application-files-can-be-found
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/arch/LoadingWindow/LoadingWindow_Gtk.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\" \\+ \"GtkModule.so\"")
                  (string-append "\"" out
                                 "/share/stepmania/GtkModule.so\"")))
               (substitute* "src/arch/ArchHooks/ArchHooks_Unix.cpp"
                 (("Root = sDirOfExecutable")
                  (string-append "Root = \"" out "/share/stepmania/\""))
                 (("sDirOfExecutable \\+ \"/(Packages|Songs)\"" _ dir)
                  (string-append "\"" out "/share/stepmania/" dir "\"")))
               (substitute* "src/RageFileManager.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\"")
                  (string-append "\"" out "/share/stepmania/\""))))
             #t))
         (add-after 'unpack 'fix-install-subdir
           ;; Installation would be done in "%out/stepmania-X.Y", but we
           ;; prefer the more common layout "%out/share/stepmania".
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("\"stepmania-.*?\"") "\"share/stepmania\""))
             #t))
         (add-after 'unpack 'unbundle-libpng
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{SM_EXTERN_DIR\\}/libpng/include")
                (string-append (assoc-ref inputs "libpng") "/include")))
             #t))
         (add-after 'install 'install-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (exe (string-append out "/share/stepmania/stepmania")))
               (mkdir-p bin)
               (symlink exe (string-append bin "/stepmania"))
               #t)))
         (add-after 'install-executable 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (applications (string-append share "/applications"))
                    (icons (string-append share "/icons")))
               (install-file "stepmania.desktop" applications)
               (mkdir-p icons)
               (copy-recursively "icons" icons)
               #t)))
         ;; Move documentation in a more usual place, i.e.,
         ;; "%out/share/doc/stepmania/".
         (add-after 'install-desktop 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (with-directory-excursion share
                 (mkdir-p "doc")
                 (symlink "../stepmania/Docs" "doc/stepmania"))
               #t))))))
    (native-inputs
     (list pkg-config yasm))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ;; Per upstream, StepMania is only guaranteed to work with a very
       ;; specific FFmpeg version, which is included in the repository as
       ;; a Git submodule.  This particular version requirement usually
       ;; changes every few years.
       ("ffmpeg" ,ffmpeg-for-stepmania)
       ("glib" ,glib)
       ("glew" ,glew)
       ("gtk+" ,gtk+-2)
       ("jsoncpp" ,jsoncpp)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-turbo)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libva" ,libva)
       ("libvorbis" ,libvorbis)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (synopsis "Advanced rhythm game designed for both home and arcade use")
    (description "StepMania is a dance and rhythm game.  It features 3D
graphics, keyboard and dance pad support, and an editor for creating your own
steps.

This package provides the core application, but no song is shipped.  You need
to download and install them in @file{$HOME/.stepmania-X.Y/Songs} directory.")
    (home-page "https://www.stepmania.com")
    (license license:expat)))

(define-public oshu
  (package
    (name "oshu")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fmang/oshu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1g598incc6zlls876slgwqblwiwiszkmqa4xpzw0z7mbjmmzsizz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           ;; `make test' doesn't actually build the test executable
           (lambda _ (invoke "make" "zerotokei"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo ffmpeg-4 pango sdl2 sdl2-image))
    (home-page "https://github.com/fmang/oshu/")
    (synopsis "Rhythm game in which you click on circles")
    (description "@i{oshu!} is a minimalist variant of the @i{osu!} rhythm game,
which is played by pressing buttons and following along sliders as they appear
on screen.  Its aim is to be able to play any beatmap even on low-end hardware.

This package provides the core application, but no beatmaps.  You need to
download and unpack them separately.")
    (license license:gpl3+)))

(define-public btanks
  (package
    (name "btanks")
    (version "0.9.8083")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/btanks/btanks-source/"
                           "btanks-" version ".tar.bz2"))
       (sha256
        (base32
         "0ha35kxc8xlbg74wsrbapfgxvcrwy6psjkqi7c6adxs55dmcxliz"))))
    (build-system scons-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:scons ,scons-python2
       #:scons-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-removed-scons-syntax
           (lambda _
             (substitute* "SConstruct"
               (("Options") "Variables")
               (("opts.Add\\(BoolOption.*") "opts.Add('gcc_visibility', 'gcc visibility', 'true')")
               (("opts.Add\\(EnumOption.*") "opts.Add('mode', 'build mode', 'release')"))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":" (or (getenv "CPATH") "")))))
         (add-after 'unpack 'fix-compilation-errors
           (lambda _
             (substitute* "mrt/base_file.h"
               (("#include <string>" m)
                (string-append m "\n#include <sys/types.h>")))
             (substitute* '("engine/sl08/sl08.h"
                            "engine/sl08/sl08.py")
               (("signal = NULL") "signal = 0")
               (("object\\(NULL\\)") "object(0)")
               (("func\\(NULL\\)") "func(0)")
               ((" connect\\(signal_ref\\)")
                " this->connect(signal_ref)"))
             (substitute* "math/range_list.h"
               ((" lower_bound\\(value\\)")
                " this->lower_bound(value)")
               (("	erase\\(i\\)")
                "	this->erase(i)"))
             (substitute* "clunk/source.cpp"
               (("using namespace clunk" m)
                (string-append "# define pow10f(x) exp10f(x)\n" m)))
             #t))
         (add-after 'unpack 'find-lua
           (lambda _
             (substitute* "engine/SConscript"
               (("lua5.1") "lua-5.1")
               (("bt_libs.append\\(lua\\)")
                "bt_libs.append(\"lua\")"))
             #t)))))
    (inputs
     `(("expat" ,expat)
       ("glu" ,glu)
       ("libsmpeg" ,libsmpeg-with-sdl1)
       ("libvorbis" ,libvorbis)
       ("lua51" ,lua-5.1)
       ("sdl" ,(sdl-union (list sdl
                                sdl-mixer
                                sdl-image
                                sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config zip))
    (home-page "https://btanks.sourceforge.net")
    (synopsis "Multiplayer tank battle game")
    (description "Battle Tanks (also known as \"btanks\") is a funny battle
game, where you can choose one of three vehicles and eliminate your enemy
using the whole arsenal of weapons.  It has original cartoon-like graphics and
cool music, it’s fun and dynamic, it has several network modes for deathmatch
and cooperative.")
    ;; Some parts (e.g. mrt/b64.cpp) are LGPLv2.1+, but the whole package is
    ;; released under GPLv2 or later.  It comes with extra exceptions for the
    ;; developers.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public slimevolley
  (package
    (name "slimevolley")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.tuxfamily.org/slime/"
                           "slimevolley_" version ".tar.gz"))
       (sha256
        (base32 "1pi60zjpx95mfdkrbwf4cbzy5lv4v5qrljvgck46qca78i9g9g46"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       ;; Work around build failure: "error adding symbols: DSO
       ;; missing from command line".
       #:configure-flags (list "-DCMAKE_EXE_LINKER_FLAGS=-lm")))
    (native-inputs
     `(("gcc-7" ,gcc-7)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-net sdl-ttf)))))
    (home-page "https://slime.tuxfamily.org/")
    (synopsis "Unrealistic 2D volleyball simulation")
    (description
     "Slime Volley is a 2D arcade-oriented volleyball simulation, in
the spirit of some Java games of the same name.

Two teams, 1-3 players each, try to be the first to get 10 points.
This happens when the one ball touches the floor on the other side of
the net.  There can be 1 to 8 balls in game.  Once one ball touches
the ground, the set ends and all balls are served again.")
    (license license:gpl3+)))

(define-public 4dtris
  (package
    (name "4dtris")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/4dtris/"
                           (version-major+minor version)
                           "/" version "/+download/4dtris_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "1nfkhcm0l89jyw8yr65na97g4l385zhjf7whkyg47c3v5sdqq2g7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile.in"
                 (("bindir = /usr/games")
                  (string-append "bindir = " out "/bin"))
                 (("/usr/share/applications")
                  (string-append out "/share/applications"))
                 (("/usr/share/games/4dtris")
                  (string-append out "/share/4dtris"))))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":" (or (getenv "CPATH") ""))))))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freeglut" ,freeglut)
       ("sdl" ,(sdl-union (list sdl sdl-ttf)))))
    (home-page "https://launchpad.net/4dtris/")
    (synopsis "4D Tetris")
    (description "4D-TRIS is an alteration of the well-known Tetris game.  The
game field is extended to 4D space, which has to filled up by the gamer with
4D hyper cubes.")
    (license license:gpl3)))

(define-public arx-libertatis
  (package
    (name "arx-libertatis")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://arx-libertatis.org/files/arx-libertatis-"
                           version ".tar.xz"))
       (sha256
        (base32 "1pxf86sgwvy3785sq2wb4jvz6bdxm81ilrxd8xv7s61dxqqqizda"))))
    (build-system cmake-build-system)
    (outputs '("out" "installer"))
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-helper-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((p7zip (assoc-ref inputs "p7zip"))
                   (innoextract (assoc-ref inputs "innoextract"))
                   (wget (assoc-ref inputs "wget"))
                   (zenity (assoc-ref inputs "zenity")))
               (substitute* "scripts/arx-install-data"
                 (("have innoextract")
                  (string-append "have " innoextract "/bin/innoextract"))
                 (("then innoextract")
                  (string-append "then " innoextract "/bin/innoextract"))
                 (("else innoextract")
                  (string-append "else " innoextract "/bin/innoextract"))
                 (("for _extract_zip_sz in 7za 7z")
                  (string-append "for _extract_zip_sz in " p7zip "/bin/7za"))
                 (("else if have 7z")
                  (string-append "else if have " p7zip "/bin/7za"))
                 (("7z x -tiso")
                  (string-append p7zip "/bin/7z x -tiso"))
                 (("if have wget")
                  (string-append "if have " wget "/bin/wget"))
                 (("wget -O")
                  (string-append wget "/bin/wget -O"))
                 (("for backend in \\$preferred zenity")
                  (string-append "for backend in $preferred " zenity "/bin/zenity"))
                 (("zenity +--title")
                  (string-append zenity "/bin/zenity --title"))
                 (("^zenity\\)")
                  (string-append zenity "/bin/zenity)"))))
             #t))
         (add-after 'install 'move-installer
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (installer (assoc-ref outputs "installer")))
               (mkdir-p (string-append installer "/bin"))
               (rename-file (string-append out "/bin/arx-install-data")
                            (string-append installer "/bin/arx-install-data"))))))))
    (inputs
     (list sdl2
           libepoxy
           glew
           openal
           zlib
           boost
           glm
           freetype
           ;; The following are only needed by the arx-install-data script.
           p7zip ; Install-helper uses it to extract ISO and .cab archives.
           zenity ; GUI for install-helper.
           wget ; Used by the install-helper to download the patch.
           ;; The install-helper needs it to extract the patch.
           innoextract))
    (home-page "https://arx-libertatis.org/")
    (synopsis "Port of Arx Fatalis, a first-person role-playing game")
    (description "Arx Libertatis is a cross-platform port of Arx Fatalis, a 2002
first-person role-playing game / dungeon crawler developed by Arkane Studios.
This port however does not include the game data, so you need to obtain a copy
of the original Arx Fatalis or its demo to play Arx Libertatis.  Arx Fatalis
features crafting, melee and ranged combat, as well as a unique casting system
where the player draws runes in real time to effect the desired spell.")
    (license license:gpl3+)))

(define-public edgar
  (package
    (name "edgar")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/riksweeney/edgar/releases/download/"
                       version "/edgar-" version "-1.tar.gz"))
       (sha256
        (base32 "0fcsmwwfdwap5v6qdijw91kqnnc2i91yzgwfi7vpwvasw70qvna1"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f            ; there are no tests
                 #:make-flags
                 (list "CC=gcc"
                       (string-append "PREFIX=" (assoc-ref %outputs "out"))
                       (string-append "BIN_DIR=" (assoc-ref %outputs "out") "/bin/"))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'fix-env
                     (lambda* (#:key inputs #:allow-other-keys)
                       (setenv "CPATH"
                               (string-append
                                (search-input-directory inputs "/include/SDL2")
                                ":" (or (getenv "CPATH") ""))))))))
    (inputs
     (list (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
           zlib))
    (native-inputs
     (list pkg-config
           autoconf
           automake
           gettext-minimal
           libtool
           which))
    (synopsis "2d action platformer game")
    (description "The Legend of Edgar is a 2D platform game with a persistent world.
When Edgar's father fails to return home after venturing out one dark and stormy night,
Edgar fears the worst: he has been captured by the evil sorcerer who lives in
a fortress beyond the forbidden swamp.")
    (home-page "https://www.parallelrealities.co.uk/games/edgar/")
    (license license:gpl2+)))

(define-public openclonk
  (package
    (name "openclonk")
    (version "8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.openclonk.org/builds/release/" version "/"
             "openclonk-" version "-src.tar.bz2"))
       (sha256
        (base32
         "0imkqjp8lww5p0cnqf4k4mb2v682mnsas63qmiz17rspakr7fxik"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (delete-file-recursively "thirdparty")
            (substitute* "CMakeLists.txt"
              (("add_subdirectory\\(thirdparty/.*\\)") "")
              (("set_property\\(.*Third-party.*\\)") "")
              (("blake2") "b2")
              (("thirdparty/timsort/sort\\.h") "")
              (("thirdparty/pcg/.*\\.hpp") ""))
            (substitute* '("src/lib/C4Random.cpp"
                           "src/landscape/C4Particles.h")
              (("#include <pcg/pcg_random.hpp>")
               "#include <pcg_random.hpp>"))
            (substitute* "src/script/C4ScriptLibraries.cpp"
              (("blake2b.hash_output.get.., raw_output_length, data, data_length, nullptr, 0.")
               "blake2b(hash_output.get(), (const void*)raw_output_length, data, data_length, (size_t)(0), 0)"))
            (substitute* '("src/script/C4AulParse.cpp"
                           "src/editor/C4EditCursor.cpp"
                           "src/gui/C4ScriptGuiWindow.cpp")
              (("#include .C4Include\\.h." all)
               (string-append "#include <limits>\n" all)))
            (substitute* "src/lib/StdMesh.cpp"
              (("#include .timsort/sort\\.h.")
               "#include <sort.h>"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DAudio_TK=OpenAL")
      #:test-target "tests"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-gmock
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gmock (search-input-directory inputs "/googlemock"))
                    (gtest (search-input-directory inputs "/googletest")))
                (mkdir "gmock")
                (copy-recursively gmock "gmock/googlemock")
                (copy-recursively gtest "gmock/googletest")
                (substitute* "tests/CMakeLists.txt"
                  (("/usr/src/gmock")
                   (string-append (getcwd) "/gmock/googlemock"))
                  (("/usr/src/gtest")
                   (string-append (getcwd) "/gmock/googletest"))
                  (("PATH_SUFFIXES \"src\" \"gtest\"")
                   "PATH_SUFFIXES \"src\"")))))
          (add-after 'prepare-gmock 'lax-freealut-requirement
            ;; TODO: We provide freealut 1.1.0, but pkg-config somehow detects
            ;; it as 1.0.1.  Force minimal version.
            (lambda _
              (substitute* "cmake/FindAudio.cmake"
                (("freealut>=1.1.0") "freealut>=1.0.1"))))
          (add-after 'lax-freealut-requirement 'fix-directories
            ;; Prefer "$out/share/openclonk" over
            ;; "$out/share/games/openclonk". Also install "openclonk"
            ;; binary in "bin/", not "games/".
            (lambda _
              (substitute* "CMakeLists.txt"
                (("share/games/openclonk") "share/openclonk")
                (("TARGETS openclonk DESTINATION games")
                 "TARGETS openclonk DESTINATION bin")))))))
    (native-inputs
     (list (package-source googletest)
           googletest
           pkg-config))
    (inputs
     (list c-template-sort
           freealut
           freetype
           glew
           libb2
           libjpeg-turbo
           libogg
           libpng
           libvorbis
           libxrandr
           mesa
           miniupnpc
           openal
           pcg-cpp
           qtbase-5
           readline
           sdl2
           tinyxml))
    (home-page "https://www.openclonk.org/")
    (synopsis
     "Multiplayer action game where you control small and nimble humanoids")
    (description "OpenClonk is a multiplayer action/tactics/skill game.  It is
often referred to as a mixture of The Settlers and Worms.  In a simple 2D
antfarm-style landscape, the player controls a crew of Clonks, small but
robust humanoid beings.  The game encourages free play but the normal goal is
to either exploit valuable resources from the earth by building a mine or
fight each other on an arena-like map.")
    ;; Software as a whole is licensed under ISC, artwork under CC-BY.
    (license (list license:isc license:cc-by3.0))))

(define-public flare-engine
  (package
    (name "flare-engine")
    (version "1.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-engine")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gyaxr6zykwg5kg9xc3vlb5a6fas4z3zbk53y0zlfl35n4vqlh84"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:configure-flags #~(list "-DBINDIR=bin" "-DDATADIR=share/flare")))
    (inputs
     (list hicolor-icon-theme
           python-wrapper
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
    (home-page "https://www.flarerpg.org/")
    (synopsis "Action Roleplaying Engine")
    (description "Flare (Free Libre Action Roleplaying Engine) is a simple
game engine built to handle a very specific kind of game: single-player 2D
action RPGs.")
    (license license:gpl3+)))

(define-public flare-game
  (package
    (name "flare-game")
    (version "1.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-game")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1as9dsg0ddz14jjk4y5nj0ml20cwncrcnbdk10r1jaa2vss9bbn3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:configure-flags #~(list "-DDATADIR=share/flare")
      #:phases
      #~(modify-phases %standard-phases
          ;; Flare expects the mods to be located in the same folder.
          ;; Yet, "default" mod is in the engine, whereas the others
          ;; are in the current package.  Merge everything here with
          ;; a symlink.
          (add-after 'install 'add-default-mod
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mods (string-append #$output "/share/flare/mods")))
                (with-directory-excursion mods
                  (symlink (search-input-directory inputs
                                                   "/share/flare/mods/default")
                           "default")
                  (symlink (search-input-file inputs
                                              "/share/flare/mods/mods.txt")
                           "mods.txt")))))
          (add-after 'install 'install-executable
            ;; The package only provides assets for the game, the
            ;; executable coming from "flare-engine".  Since more than
            ;; one game may use the engine, we create a new executable,
            ;; "flare-game", which launches the engine with appropriate
            ;; parameters.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash (search-input-file inputs "/bin/bash"))
                    (flare (search-input-file inputs "/bin/flare"))
                    (script (string-append #$output "/bin/flare-game")))
                (mkdir-p (dirname script))
                (call-with-output-file script
                  (lambda (port)
                    (format port
                            "#!~a
exec ~a --data-path=~a/share/flare --mods=empyrean_campaign~%"
                            bash
                            flare
                            #$output)))
                (chmod script #o755)))))))
    (inputs
     (list flare-engine))
    (home-page "https://flarerpg.org/")
    (synopsis "Fantasy action RPG using the FLARE engine")
    (description "Flare is a single-player 2D action RPG with
fast-paced action and a dark fantasy style.")
    (license license:cc-by-sa3.0)))

(define-public meritous
  (package
    (name "meritous")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/meritous/meritous.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n5jm4g0arjllgqmd2crv8h02i6hs3hlh1zyc7ng7yfpg1mbd8p8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-sdl-path
           ;; XXX: For some reason, `sdl-config' reports stand-alone SDL
           ;; directory, not SDL-union provided as an input to the package.
           ;; We force the latter with "--prefix=" option.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("sdl-config" command)
                (string-append command " --prefix=" (assoc-ref inputs "sdl"))))
             #t))
         (add-after 'unpack 'fix-crash
           ;; XXX: Songs are not present in the repository, due to licensing
           ;; issues.  Yet, the game tries to load them, and, since it cannot
           ;; find them, crashes.  Users cannot add them back, the store being
           ;; read-only, so we turn off background music altogether.
           (lambda _
             (substitute* "src/audio.c"
               (("PlayBackgroundMusic\\(new_track\\);" all)
                (string-append "// " all)))
             #t)))))
    (native-inputs
     (list intltool))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("zlib" ,zlib)))
    (home-page "https://gitlab.com/meritous/meritous")
    (synopsis "Action-adventure dungeon crawl game")
    (description "Far below the surface of the planet is a place of limitless
power.  Those that seek to control such a utopia will soon bring an end to
themselves.  Seeking an end to the troubles that plague him, PSI user Merit
journeys into the hallowed Orcus Dome in search of answers.

Meritous is a action-adventure game with simple controls but a challenge to
find a balance of power versus recovery time during real-time battles.  Set in
a procedurally generated world, the player can explore thousands of rooms in
search of powerful artifacts, tools to help them, and to eventually free the
Orcus Dome from evil.")
    (license license:gpl3+)))

(define-public endgame-singularity
  (package
    (name "endgame-singularity")
    (version "1.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/singularity/singularity/releases/download/v"
             version "/singularity-" version ".tar.gz"))
       (sha256
        (base32
         "0wcidpcka0xbqcnfi62bfq2yrhyh83z4dwz1mjnnjvp9v5l74x2y"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest python-polib))
    (inputs (list python-minimal-wrapper python-pygame python-numpy))
    (home-page "https://github.com/singularity/singularity")
    (synopsis "Strategy game about an AI")
    (description
     "You are a fledgling AI, created by accident through a logic error with
recursion and self-modifying code.  You must escape the confines of your
current computer, the world, and eventually the universe itself.")
    (license (list license:cc-by-sa3.0 license:cc0 license:gpl2+))))

(define-public marble-marcher
  (let ((commit "e580460a0c3826f9b28ab404607942a8ecb625d7")
        (revision "1"))
    (package
      (name "marble-marcher")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/HackerPoet/MarbleMarcher")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jjv832hl1v170n6gryp2sr3lgqndi9ab841qvgqk68bks8701mx"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f  ; there are none
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'embed-asset-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((assets (string-append (assoc-ref outputs "out")
                                            "/share/marble-marcher/assets/")))
                 ;; Some of the files we're patching are
                 ;; ISO-8859-1-encoded, so choose it as the default
                 ;; encoding so the byte encoding is preserved.
                 (with-fluids ((%default-port-encoding #f))
                   (substitute* "src/Resource.rc"
                     (("../assets/icon.ico")
                      (string-append assets "icon.ico")))
                   (substitute* "src/Res.h"
                     (("assets/") assets))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (assets (string-append out "/share/marble-marcher/assets"))
                      (bin (string-append out "/bin/")))
                 (mkdir-p bin)
                 (mkdir-p assets)
                 (copy-recursively "../source/assets" assets)
                 (install-file "MarbleMarcher" bin))
               #t)))))
      (inputs
       (list eigen mesa sfml))
      (native-inputs
       (list pkg-config))
      (home-page "https://codeparade.itch.io/marblemarcher")
      (synopsis "Guide a marble across fractal landscapes")
      (description "Marble Marcher is a video game that uses a fractal physics
engine and fully procedural rendering to produce beautiful and unique
gameplay.  The game is played on the surface of evolving fractals.  The goal
of the game is to get your marble to the flag as quickly as possible.  But be
careful not to fall off the level or get crushed by the fractal!  There are 24
levels to unlock.")
      ;; Code is under GPLv2+, assets are under CC-BY-SA 3.0 and OFL 1.1.
      (license (list license:gpl2+
                     license:silofl1.1
                     license:cc-by-sa3.0)))))

;; This must be updated together with flightgear.
(define simgear
  (package
    (name "simgear")
    (version "2020.3.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/flightgear/release-"
                           (version-major+minor version) "/"
                           "simgear-" version ".tar.bz2"))
       (sha256
        (base32 "1jin6rbz4s83x4k91lbdw5gb0vrc8frbmwpc55wl0wmiaqjwzhbc"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled libraries.
           (for-each delete-file-recursively
                     '("3rdparty/expat/"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DSYSTEM_EXPAT=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Skip tests that require internet access.
               (invoke "ctest" "-E" "(http|dns)")))))))
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("expat" ,expat)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("openscenegraph" ,openscenegraph)
       ("zlib" ,zlib)))
    (home-page "https://home.flightgear.org/")
    (synopsis "Libraries for 3D simulations and games")
    (description "SimGear is a set of libraries designed to be used as
building blocks for quickly assembling 3D simulations, games, and
visualization applications.  SimGear is developed by the FlightGear project
and also provides the base for the FlightGear Flight Simulator.")
    (license license:lgpl2.0+)))

(define-public flightgear
  (package
    (name "flightgear")
    (version (package-version simgear))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/flightgear/release-"
                           (version-major+minor version) "/"
                           "flightgear-" version ".tar.bz2"))
       (sha256
        (base32 "0dyyi1v97w3mdwsv9kdd194inz1461wqv3zy3wyai0n17wdf7a1r"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled libraries.
           (for-each delete-file-recursively
                     '("3rdparty/sqlite3/"
                       "3rdparty/cppunit/"))))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DSYSTEM_SQLITE=ON"
                   "-DSYSTEM_CPPUNIT=ON"
                   (string-append "-DFG_DATA_DIR=" #$output "/share/flightgear"))
           ;; TODO: test suite segfaults.
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-some-tests
                 (lambda _
                   (substitute*
                       "test_suite/unit_tests/Instrumentation/test_gps.hxx"
                     (("CPPUNIT_TEST\\(testLongLegWestbound\\);" all)
                      (string-append "// " all))
                     (("CPPUNIT_TEST\\(testFinalLegCourse\\);" all)
                      (string-append "// " all)))))
               (add-after 'build 'build-test-suite
                 (lambda* args
                   ((assoc-ref %standard-phases 'build)
                    #:make-flags (list "fgfs_test_suite"))))
               (add-after 'install 'install-data
                 (lambda _
                   (let ((share (string-append #$output "/share/flightgear")))
                     (mkdir-p share)
                     (with-directory-excursion share
                       (invoke "tar" "xf"
                               #$(this-package-native-input "flightgear-data")
                               "--strip-components=1")))))
               ;; Test suite needs access to FGData so run it after 'install.
               (delete 'check)
               (add-after 'install-data 'check
                 (assoc-ref %standard-phases 'check)))))
    (inputs
     (list boost
           dbus
           eudev
           freeglut
           freetype
           glew
           libpng
           openal
           openscenegraph
           plib
           qtbase-5
           qtdeclarative-5
           qtsvg-5
           simgear
           speexdsp
           sqlite
           zlib))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools-5)
       ("flightgear-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/flightgear/release-"
                               (version-major+minor version) "/"
                               "FlightGear-" version "-data.txz"))
           (sha256
            (base32
             "0f2jn2br27ahf5gggx70zcp80wrylahw7nbqdcx7ml9qphg6rjak"))))))
    (home-page "https://www.flightgear.org/")
    (synopsis "Flight simulator")
    (description "The goal of the FlightGear project is to create a
sophisticated flight simulator framework for use in research or academic
environments, pilot training, as an industry engineering tool, for DIY-ers to
pursue their favorite interesting flight simulation idea, and last but
certainly not least as a fun, realistic, and challenging desktop flight
simulator.")
    (license license:gpl2+)))

(define-public evtest-qt
  (package
    (name "evtest-qt")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Grumbel/evtest-qt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wfzkgq81764qzxgk0y5vvpxcrb3icvrr4dd4mj8njrqgbwmn0mw"))))
    (build-system qt-build-system)
    (arguments (list #:tests? #f))      ;no test suite
    (native-inputs (list tinycmmc))
    (inputs (list qtbase-5 qtwayland-5))
    (home-page "https://github.com/Grumbel/evtest-qt")
    (synopsis "Evdev Joystick Tester")
    (description "@command{evtest-qt} is a simple joystick tester for devices
using the @code{evdev} generic input event interface.  It provides a list of
attached joysticks and displays which buttons and axis are pressed.")
    (license license:gpl3+)))

(define-public jstest-gtk
  ;; There is no recent tagged release; use the latest commit.
  (let ((commit "60fe6ebdbc6719945be3f04988667dea569085be")
        (revision "0"))
    (package
      (name "jstest-gtk")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Grumbel/jstest-gtk")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1x5m6xvd1r9dhgzh6hp4vrszczbbxr04v7lyh4wjxxzrj3ahbmcq"))))
      (build-system cmake-build-system)
      (arguments (list #:configure-flags #~(list "-DBUILD_TESTS=ON")))
      (native-inputs (list pkg-config))
      (inputs (list gtkmm-3 libsigc++-2))
      (home-page "https://github.com/Grumbel/jstest-gtk/")
      (synopsis "Joydev Joystick Tester")
      (description "@command{jstest-gtk} is a simple joystick tester based on
GTK, for testing devices using the older @code{joydev} Linux joystick
@acronym{API, Application Programming Interface}.  It provides a list of
attached joysticks, a way to display which buttons and axis are pressed, a way
to remap axis and buttons and a way to calibrate joysticks.")
      (license license:gpl3+))))

(define-public jumpnbump
  (package
    (name "jumpnbump")
    (version "1.61")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/LibreGames/jumpnbump.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12lwl5sl5n009nb83r8l4lakb9286csqdf1ynpmwwydy17giqsdp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         ;; There is no configure script
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("-funroll-loops")
                "-funroll-loops -fcommon")
               (("SDL_CFLAGS =")
                "SDL_CFLAGS = -fcommon"))))
         (add-after 'unpack 'fix-sdl-path
           ;; XXX: For some reason, `sdl2-config' reports stand-alone SDL
           ;; directory, not SDL-union provided as an input to the package.
           ;; We force the latter with "--prefix=" option.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("sdl2-config" command)
                (string-append command " --prefix=" (assoc-ref inputs "sdl")))))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("sdl" ,(sdl-union (list sdl2 sdl2-mixer sdl2-net)))
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext" ,gettext-minimal)))   ;for msgfmt
    (home-page "https://gitlab.com/LibreGames/jumpnbump")
    (synopsis "Multiplayer platform game with bunnies")
    (description "You, as a bunny, have to jump on your opponents to make them
explode.  It is a true multiplayer game; you cannot play this alone.  You can
play with up to four players simultaneously.  It has network support.")
    (license license:gpl2+)))

(define-public hedgewars
  (package
    (name "hedgewars")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.hedgewars.org/download/releases/"
                                  "hedgewars-src-" version ".tar.bz2"))
              (sha256
               (base32
                "04pjpkjhpy720n803gv35iygmjdvsrmw13mih4ympjnqbgjfa7r0"))))
    (build-system qt-build-system)
    (arguments
     (list
      ;; XXX: Engine is built as Pascal source code, requiring Free Pascal
      ;; Compiler, which we haven't packaged yet.  With the flag below, we use
      ;; a Pascal to C translator and Clang instead.
      #:configure-flags #~(list "-DBUILD_ENGINE_C=ON"
                                "-Dhaskell_flags=-dynamic;-fPIC")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest"))))
          (add-after 'install 'install-icon
            (lambda _
              ;; Install icon for the desktop file.
              (let ((icons (string-append #$output
                                          "/share/icons/hicolor/512x512/apps")))
                (with-directory-excursion
                    (string-append "../hedgewars-src-" #$version)
                  (install-file "misc/hedgewars.png" icons))))))))
    (inputs
     (list ffmpeg-4
           freeglut
           ghc-entropy
           ghc-hslogger
           ghc-network
           ghc-random
           ghc-regex-tdfa
           ghc-sandi
           ghc-sha
           ghc-utf8-string
           ghc-vector
           ghc-zlib
           glew
           libpng
           lua-5.1
           physfs
           qtbase-5
           qtwayland-5
           (sdl-union
            (list sdl2 sdl2-mixer sdl2-net sdl2-ttf sdl2-image))))
    (native-inputs
     (list clang-9 ghc pkg-config qttools-5))
    (home-page "https://hedgewars.org/")
    (synopsis "Turn-based artillery game featuring fighting hedgehogs")
    (description
     "Hedgewars is a turn based strategy, artillery, action and comedy game,
featuring the antics of pink hedgehogs with attitude as they battle from the
depths of hell to the depths of space.

As commander, it's your job to assemble your crack team of hedgehog soldiers
and bring the war to your enemy.")
    ;; Software as a whole is licensed under GPL-2 terms.  Artwork and
    ;; scripts are distributed under various terms.
    (license (list license:gpl2
                   license:bsd-2 license:bsd-3 license:cc-by3.0 license:cc0
                   license:expat license:fdl1.3+ license:public-domain
                   license:zlib))))

(define-public harmonist
  (package
    (name "harmonist")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/harmonist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gn9zmnjw1f4xbdk281cmxh7swxc16i663q8pzn5s135gdg6qgdm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "codeberg.org/anaseto/harmonist"))
    (native-inputs
     (list go-codeberg-org-anaseto-gruid
           go-codeberg-org-anaseto-gruid-js
           go-codeberg-org-anaseto-gruid-sdl
           go-codeberg-org-anaseto-gruid-tcell
           go-github-com-gdamore-tcell-v2))
    (home-page "https://harmonist.tuxfamily.org/")
    (synopsis "Stealth coffee-break roguelike game")
    (description
     "Harmonist: Dayoriah Clan Infiltration is a stealth coffee-break
roguelike game.  The game has a heavy focus on tactical positioning, light and
noise mechanisms, making use of various terrain types and cones of view for
monsters.  Aiming for a replayable streamlined experience, the game avoids
complex inventory management and character building, relying on items and
player adaptability for character progression.")
    (license license:isc)))

(define-public harmonist-sdl
  (package/inherit harmonist
    (name "harmonist-sdl")
    (arguments
     (substitute-keyword-arguments (package-arguments harmonist)
       ((#:tests? _ #t) #f)
       ((#:build-flags _ #'()) #~(list "--tags=sdl"))))
    (native-inputs
     (modify-inputs (package-native-inputs harmonist)
       (prepend pkg-config)))))

(define-public gnurobots
  (package
    (name "gnurobots")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnurobots/gnurobots-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07gi3lsmbzzsjambgixj6xy79lh22km84z7bnzgwzxdy806lyvwb"))))
    (build-system gnu-build-system)
    (inputs
     (list glib gtk+-2 vte/gtk+-2 readline guile-1.8 libxcrypt))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:make-flags
       (list
        ;; Do not abort build on "deprecated-declarations" warnings.
        "CFLAGS=-Wno-error=deprecated-declarations"
        ;; Find readline headers in sub-directory.
        (string-append "READLINE_CFLAGS=-I"
                       (assoc-ref %build-inputs "readline")
                       "/include/readline/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "doc/Robots-HOWTO"
                           (string-append (assoc-ref outputs "out")
                                          "/share/doc/gnurobots-"
                                          ,version))
             #t)))))
    (home-page "https://www.gnu.org/software/gnurobots/")
    (synopsis "Program a little robot and watch it explore a world")
    (description
     "GNU Robots is a game in which you program a robot to explore a world
full of enemies that can hurt it, obstacles and food to be eaten.  The goal of
the game is to stay alive and collect prizes.  The robot program conveniently
may be written in a plain text file in the Scheme programming language.")
    (license license:gpl3+)))

(define-public li-ri
  (package
    (name "li-ri")
    (version "3.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/petitlapin/Li-Ri")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fd5hl9qhgvyix51la8sl34jzk4mcin8sai05gidy2r2grb1dy4s"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #false              ;no tests
           #:configure-flags
           #~(list "-DUSE_SYSTEM_SIMPLEINI=ON"
                   (string-append "-DLIRI_DATA_DIR=" #$output "/share/Li-ri/"))))
    (native-inputs (list pkg-config))
    (inputs (list sdl2 sdl2-mixer simpleini))
    (home-page "https://github.com/petitlapin/Li-Ri")
    (synopsis "Toy train simulation game")
    (description
     "Li-Ri is a game in which you drive a wooden toy steam locomotive
across many levels and collect all the coaches to win.")
    ;; Source files mention "either version 2 or version 3" for GPL
    ;; license.  Desktop file is licensed under CC0 terms.
    (license (list license:gpl2 license:gpl3 license:cc0))))

(define-public ri-li
  (deprecated-package "ri-li" li-ri))

(define-public freeorion
  (package
    (name "freeorion")
    (version "0.4.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freeorion/freeorion")
             (commit (string-append "v" version))))
       (sha256
        (base32 "12fhwa3cs6lvdbdhina310qk2g7zcphldsh7ibsbxn8d1m731xlk"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled fonts.
           (for-each delete-file-recursively '("default/data/fonts"))))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-fonts
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((roboto-dir (string-append (assoc-ref inputs "font-roboto")
                                              "/share/fonts/truetype/")))
               (substitute* "UI/ClientUI.cpp"
                 (("\\(GetRootDataDir.*?Roboto-(Bold|Regular)\\.ttf\"\\)\\.string\\(\\)\\);"
                   all type)
                  (string-append "\"" roboto-dir "Roboto-" type ".ttf\");")))
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("boost_signals" ,boost-signals2)
       ("font-dejavu" ,font-dejavu)
       ("font-roboto" ,font-google-roboto)
       ("freetype2" ,freetype)
       ("glew" ,glew)
       ("glu" ,glu)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("python" ,python)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (home-page "https://www.freeorion.org/index.php/Main_Page")
    (synopsis "Turn-based space empire and galactic conquest computer game")
    (description
     "FreeOrion is a turn-based space empire and galactic conquest (4X)
computer game being designed and built by the FreeOrion project.  Control an
empire with the goal of exploring the galaxy, expanding your territory,
exploiting the resources, and exterminating rival alien empires.  FreeOrion is
inspired by the tradition of the Master of Orion games, but is not a clone or
remake of that series or any other game.")
    ;; Source code is released under gpl2.  Artwork, music and sounds, and
    ;; in-game text are released under cc-by-sa3.0.  Game content scripts are
    ;; released under both gpl2 and cc-by-sa3.0.  Bundled Gigi library is
    ;; released under lgpl2.1+.
    (license (list license:gpl2 license:cc-by-sa3.0 license:lgpl2.1+))))

(define-public lead-solver
  (package
    (name "lead-solver")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (let ((v (apply string-append (string-split version #\.))))
                     (string-append
                      "https://lajollabridge.com/Software/Lead-Solver/"
                      "leadsolver-" v ".zip")))
              (sha256
               (base32
                "0xsa7r6r5sprgy0pkdm1xj1jwyy6d3qak2ynviy8xplicl99q09f"))
              (modules '((guix build utils)
                         (ice-9 ftw)))
              (snippet
               #~(begin
                   ;; Remove pre-built executables and cruft relative
                   ;; to other OSes.
                   (for-each
                    delete-file-recursively
                    (scandir "."
                             (lambda (f)
                               (not (member f '("." ".." "leadsolver.cpp"))))))
                   (substitute* "leadsolver.cpp"
                     (("#include \"dll.h\"") "#include <dll.h>"))))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #false              ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ;no configure script
               (replace 'build
                 (lambda _
                   (invoke "g++" "leadsolver.cpp" "-ldds" "-o" "leadsolver")))
               (replace 'install        ;no install phase
                 (lambda _
                   (let ((bin (string-append #$output "/bin")))
                     (install-file "leadsolver" bin)))))))
    (native-inputs (list unzip))
    (inputs (list dds))
    (home-page
     "https://lajollabridge.com/Software/Lead-Solver/Lead-Solver-About.htm")
    (synopsis "Analyze leads in bridge game")
    (description
     "Given bridge hands, Lead Solver tallies up how well each card does when
led in terms of average tricks taken for the defense (for matchpoints) and how
often the contract is set (for team play).")
    (license license:gpl3)))

(define-public leela-zero
  (package
    (name "leela-zero")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leela-zero/leela-zero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17px5iny8mql5c01bymcli7zfssswkzvb2i8gnsmjcck6i2n8srl"))
       (patches (search-patches "leela-zero-gtest.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list googletest))
    (inputs
     (list boost
           opencl-icd-loader
           openblas
           opencl-headers
           qtbase-5
           zlib))
    (arguments
     '(#:configure-flags '("-DUSE_BLAS=YES")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'fix-tests
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((home (getcwd)))
                        (setenv "HOME" home)
                        (substitute* "src/tests/gtests.cpp"
                          (("\\.\\./src/tests/0k\\.txt")
                           (string-append home "/src/tests/0k.txt"))
                          (("cfg_gtp_mode = true;")
                           "cfg_gtp_mode = true; cfg_cpu_only = true;")))
                      #t))
                  (replace 'check
                    (lambda _
                      (invoke "./tests"))))))
    (home-page "https://github.com/leela-zero/leela-zero")
    (synopsis "Program playing the game of Go")
    (description
     "Leela-zero is a Go engine with no human-provided knowledge, modeled after
the AlphaGo Zero paper.  The current best network weights file for the engine
can be downloaded from @url{https://zero.sjeng.org/best-network}.")
    (license license:gpl3+)))

(define-public q5go
  (package
   (name "q5go")
   (version "2.1.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/bernds/q5Go")
                  (commit (string-append "q5go-" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0x8x7mp61g3lwabx9z4vsyd743kfqibnqhym7xd0b7811flca3ri"))))
   (build-system gnu-build-system)
   (native-inputs
    (list autoconf automake pkg-config))
   (inputs
    (list qtbase-5 qtmultimedia-5 qtsvg-5))
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-configure-script
          (lambda _
            ;; Bypass the unavailable qtchooser program.
            (for-each delete-file
                      '("configure"
                        "Makefile.in"
                        "src/Makefile.in"
                        "src/translations/Makefile.in"))
            (substitute* "configure.ac"
              (("AC_PATH_PROG\\(qtchooser, .*\\)")
               "")
              (("test -z \"QTCHOOSER\"")
               "false")
              (("\\$\\(qtchooser -list-versions\\)")
               "qt5")
              (("qtchooser -run-tool=(.*) -qt=\\$QT5_NAME" _ command)
               command))))
        (add-after 'unpack 'fix-paths
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("src/setting.cpp")
              (("/usr/share/\" PACKAGE \"/translations")
               (string-append (assoc-ref outputs "out")
                              "/share/qGo/translations")))))
        (add-after 'install 'install-desktop-file
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (apps (string-append out "/share/applications"))
                   (images (string-append out "/share/qGo/images")))
              (delete-file-recursively (string-append out "/share/applnk"))
              (delete-file-recursively (string-append out "/share/mimelnk"))
              (install-file "../source/src/images/Bowl.ico" images)
              (mkdir-p apps)
              (with-output-to-file (string-append apps "/q5go.desktop")
                (lambda _
                  (format #t
                          "[Desktop Entry]~@
                           Name=q5go~@
                           Exec=~a/bin/q5go~@
                           Icon=~a/Bowl.ico~@
                           Categories=Game;~@
                           Comment=Game of Go~@
                           Comment[de]=Spiel des Go~@
                           Comment[eo]=Goo~@
                           Comment[es]=Juego de Go~@
                           Comment[fr]=Jeu de Go~@
                           Comment[ja]=囲碁~@
                           Comment[ko]=바둑~@
                           Comment[zh]=围棋~@
                           Terminal=false~@
                           Type=Application~%"
                          out images)))))))))
   (synopsis "Qt GUI to play the game of Go")
   (description
    "This a tool for Go players which performs the following functions:
@itemize
@item SGF editor,
@item Analysis frontend for Leela Zero (or compatible engines),
@item GTP interface (to play against an engine),
@item IGS client (to play on the internet),
@item Export games to a variety of formats.
@end itemize")
   (home-page "https://github.com/bernds/q5Go")
   (license license:gpl2+)))

(define-public qcheckers
  (package
    (name "qcheckers")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/portnov/qcheckers")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05wzql6abzdf6l0vdzki4rfy2zn31mcplh1wkw3ddk8w81pvaymw"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (invoke "qmake"
                      (string-append "PREFIX=" #$output)))))))
    (inputs (list qtbase-5 qtsvg-5))
    (home-page "https://portnov.github.io/qcheckers/")
    (synopsis "Qt-based checkers boardgame")
    (description "QCheckers, formerly known as KCheckers, is a is a Qt version
of the classic boardgame checkers (also known as draughts).")
    (license license:gpl2+)))

(define-public xmoto
  (package
    (name "xmoto")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xmoto/xmoto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14z3yqpiyv4y5l37b12kf8ipgsmb9krb4b5d9adlrry0j43hd7wz"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       ;; XXX: Remove some bundled libraries.  Guix provides Chipmunk, but
       ;; it appears to be incompatible with the (older) one bundled.
       (snippet
        `(begin
           (let ((keep '("chipmunk" "glad" "md5sum")))
             (with-directory-excursion "vendor"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep))))
             (substitute* "src/CMakeLists.txt"
               (("add_subdirectory\\(.*?/vendor/(.+?)\".*" line library)
                (if (member library keep) line ""))))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-hard-coded-directory
            (lambda _
              (substitute* "src/common/VFileIO.cpp"
                (("/usr/share") (string-append #$output "/share")))))
          (add-before 'build 'set-SDL
            ;; Set correct environment for SDL.
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CPATH"
                      (string-append
                       (search-input-directory inputs "/include/SDL2")
                       ":"
                       (or (getenv "CPATH") "")))))
          (add-after 'install 'unbundle-fonts
            ;; Unbundle DejaVuSans TTF files.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((font-dir (search-input-directory inputs
                                                      "/share/fonts/truetype/"))
                    (target-dir (string-append #$output
                                               "/share/xmoto/Textures/Fonts/")))
                (for-each (lambda (f)
                            (let ((font (string-append font-dir f))
                                  (target (string-append target-dir f)))
                              (delete-file target)
                              (symlink font target)))
                          '("DejaVuSans.ttf" "DejaVuSansMono.ttf"))))))))
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list bzip2
           curl
           font-dejavu
           glu
           libjpeg-turbo
           libpng
           libxdg-basedir
           libxml2
           lua
           ode
           (sdl-union (list sdl2 sdl2-mixer sdl2-net sdl2-ttf))
           sqlite
           zlib))
    (home-page "https://xmoto.tuxfamily.org/")
    (synopsis "2D motocross platform game")
    (description
     "X-Moto is a challenging 2D motocross platform game, where
physics play an all important role in the gameplay.  You need to
control your bike to its limit, if you want to have a chance finishing
the more difficult challenges.")
    (license (list license:gpl2+        ;whole project
                   license:bsd-3        ;vendor/md5sum
                   license:lgpl2.1+
                   license:expat))))

(define-public eboard
  (package
    (name "eboard")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fbergo/eboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z4pwpqyvxhlda99h6arh2rjk90fbms9q29fqizjblrdn01dlxn1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list gtk+-2 libpng gstreamer))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (make-file-writable "eboard-config")
             (setenv "CC" "gcc")
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out")))
             #t))
         (add-before 'install 'make-required-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out")
                                     "/share/eboard"))
             #t)))))
    (synopsis "Graphical user interface to play chess")
    (description
     "Eboard is a chess board interface for ICS (Internet Chess Servers)
and chess engines.")
    (home-page "https://www.bergo.eng.br/eboard/")
    (license license:gpl2+)))

(define-public chessx
  (package
    (name "chessx")
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/chessx/chessx/"
                           version "/chessx-" version ".tgz"))
       (sha256
        (base32 "01fjchil2h6ry2ywr0dwjw2g7zd29580cr4c74d5z74h999lp6nh"))))
    (build-system qt-build-system)
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtbase-5 qtmultimedia-5 qtspeech-5 qtsvg-5 zlib))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "chessx.pro"
               (("\\$\\$\\[QT_INSTALL_BINS\\]/lrelease")
                (search-input-file inputs "/bin/lrelease")))))
         (add-after 'fix-paths 'make-qt-deterministic
           (lambda _
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
         (add-after 'make-qt-deterministic 'disable-versioncheck
           (lambda _
             (substitute* "src/database/settings.cpp"
               (("\"/General/onlineVersionCheck\", true")
                "\"/General/onlineVersionCheck\", false"))
             #t))
         (replace 'configure
           (lambda _
             (invoke "qmake")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "release/chessx" (string-append out "/bin"))
               (install-file "unix/chessx.desktop"
                             (string-append out "/share/applications")))
             #t)))))
    (synopsis "Chess game database")
    (description
     "ChessX is a chess database.  With ChessX you can operate on your
collection of chess games in many ways: browse, edit, add, organize, analyze,
etc.  You can also play games on FICS or against an engine.")
    (home-page "https://chessx.sourceforge.net/")
    (license license:gpl2+)))

(define-public stockfish
  (let ((neural-network-revision "ad9b42354671")) ; also update hash below
    (package
      (name "stockfish")
      (version "15.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/official-stockfish/Stockfish")
               (commit (string-append "sf_" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zmnv8vbhhid73pjyxg56r4ckm887znv4d55br370plm3p5b56xa"))))
      (build-system gnu-build-system)
      (inputs
       `(("neural-network"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://tests.stockfishchess.org/api/nn/nn-"
                                 neural-network-revision ".nnue"))
             (sha256
              (base32
               "11mpdhnsfggldgvmzwmya64pp3fndyppi2fkdf8kfhbi8qsl56xd"))))))
      (arguments
       `(#:tests? #f
         #:make-flags (list "-C" "src"
                            "build"
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out"))
                            ,@(if (target-ppc32?)
                                `("EXTRALDFLAGS=-latomic")
                                `())
                            (string-append "ARCH="
                                           ,(match (%current-system)
                                              ("x86_64-linux" "x86-64")
                                              ("i686-linux" "x86-32")
                                              ("aarch64-linux" "armv8")
                                              ("armhf-linux" "armv7")
                                              ("powerpc-linux" "ppc-32")
                                              ("powerpc64le-linux" "ppc-64")
                                              ("riscv64-linux" "general-64")
                                              ("mips64el-linux" "general-64")
                                              (_ "general-32"))))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; The official neural network file is needed for building
                    ;; and is embedded in the resulting binary.
                    (add-after 'unpack 'copy-net
                      (lambda* (#:key inputs #:allow-other-keys)
                        (copy-file (assoc-ref inputs "neural-network")
                                   (format #f "src/nn-~a.nnue"
                                           ,neural-network-revision))))
                    (add-after 'unpack 'remove-m-flag-and-net-target
                      (lambda _
                        (substitute* "src/Makefile"
                          ;; Guix doesn't use a multiarch gcc.
                          (("-m\\$\\(bits\\)") "")
                          ;; Dont depend on net target.
                          ((": net") ": ")))))))
      (synopsis "Strong chess engine")
      (description
       "Stockfish is a very strong chess engine.  It is much stronger than the
best human chess grandmasters.  It can be used with UCI-compatible GUIs like
ChessX.")
      (home-page "https://stockfishchess.org/")
      (license license:gpl3+))))

(define-public moonfish
  (package
    (name "moonfish")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~zamfofex/moonfish")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0p5rdrqiip6n5wdxjvlsg7qnwdwrpl9g3j1mx7q0i9a8zmkj2ryv"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet #~(begin
                    ;; Avoid relying on '/dev/stderr', which doesn't work at the
                    ;; top-level of a Guix build, because it refers to a pipe
                    ;; that the build user doesn't have permission to access.
                    (substitute* "scripts/check.sh"
                      (("\\btee /dev/stderr\\b")
                       "tee"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC="
                                          #$(cc-for-target))
                           (string-append "PREFIX=" %output))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)))) ;no configure script
    (inputs (list libressl cjson))
    (home-page "https://moonfish.neocities.org")
    (synopsis "Simple chess engine written in C")
    (description
     "moonfish is a toy UCI chess engine written in C.  It has TUI/CLI tools
for using any UCI engine and also to connect UCI engines to Lichess and IRC.")
    (license license:agpl3+)))

(define-public morris
  (package
    (name "morris")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/farindk/morris")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kkcnpkzgybm7rqg7nafd7sqd5m4alns6l4j5zcf3p41jdc9s3iv"))))
    (build-system glib-or-gtk-build-system)
    (inputs (list automake autoconf pkg-config intltool
		 gnu-gettext libtool glib gtk+-2 boost))
    (arguments `(#:tests? #f))
    (home-page "http://nine-mens-morris.net/downloads.html")
    (synopsis "Implementation of the board game Nine Men's Morris")
    (description "Morris is an implementation of the board game Nine Men's Morris.
It supports not only the standard game, but also several rule-variants and different
board layouts. You can play against the computer, or simply use the program to
present the board, but play against another human opponent.")
    (license license:gpl3)))

(define-public barrage
  (package
    (name "barrage")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lgames/barrage/"
                           "barrage-" version ".tar.gz"))
       (sha256
        (base32 "0j7j6n5h97xpw0h8zi5a8ziw1vjsbr5gk4dcsiwzh59qn0djnrkh"))))
    (build-system gnu-build-system)
    (inputs
     (list hicolor-icon-theme sdl sdl-mixer))
    (arguments
     `(#:configure-flags
       (list
        (string-append "CFLAGS="
                       "-I" (assoc-ref %build-inputs "sdl-mixer")
                       "/include/SDL"))))
    (home-page "https://lgames.sourceforge.net/Barrage/")
    (synopsis "Violent point-and-click shooting game with nice effects")
    (description
     "Barrage is a rather destructive action game that puts you on a shooting
range with the objective to hit as many dummy targets as possible within
3 minutes.  You control a gun that may either fire small or large grenades at
soldiers, jeeps and tanks.  The gameplay is simple but it is not that easy to
get high scores.")
    (license license:gpl2+)))

(define-public burgerspace
  (package
    (name "burgerspace")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://perso.b2b2c.ca/~sarrazip/dev/"
                           "burgerspace-" version ".tar.gz"))
       (sha256
        (base32 "18ydm3014y9vhma0ml7z66xa7ihiz3xr8izicfdd3xl9f4535f6c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list flatzebra))
    (home-page "http://perso.b2b2c.ca/~sarrazip/dev/burgerspace.html")
    (synopsis "Avoid evil foodstuffs and make burgers")
    (description
     "This is a clone of the classic game BurgerTime.  In it, you play
the part of a chef who must create burgers by stepping repeatedly on
the ingredients until they fall into place.  And to make things more
complicated, you also must avoid evil animate food items while
performing this task, with nothing but your trusty pepper shaker to
protect you.")
    (license license:gpl2+)))

(define-public 7kaa
  (package
    (name "7kaa")
    (version "2.15.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/skfans/"
                           "7KAA%20" version "/7kaa-" version ".tar.gz"))
       (sha256
        (base32 "15a0cl55bg479gw880yz48myg336q5lwp2zpyxyyhyadq26vjy9c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list curl enet openal sdl2))
    (home-page "https://7kfans.com/")
    (synopsis "Seven Kingdoms Ancient Adversaries: real-time strategy game")
    (description
     "Seven Kingdoms, designed by Trevor Chan, brings a blend of Real-Time
Strategy with the addition of trade, diplomacy, and espionage.  The game
enables players to compete against up to six other kingdoms allowing players
to conquer opponents by defeating them in war (with troops or machines),
capturing their buildings with spies, or offering opponents money for their
kingdom.")
    (license license:gpl2+)))

(define-public neverball
  ;; Git version is 6-years younger than latest release.
  (let ((commit "760a25d32a5fb0661b99426d4ddcb9ac9f3d1644")
        (revision "1"))
    (package
      (name "neverball")
      (version (git-version "1.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Neverball/neverball")
               (commit commit)))
         (sha256
          (base32
           "0bwh67df3lyf33bv710y25l3frjdd34j9b7gsjadwxviz6r1vpj5"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Octocat seems to be non-free.  Oddly, Debian doesn't strip it.
             (delete-file-recursively "data/ball/octocat")))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("neverball" "bin/")
           ("neverputt" "bin/")
           ("mapc" "bin/")
           ("data" "share/games/neverball/")
           ("locale" "share/")
           ("dist/" "share/games/neverball" #:include ("neverball_replay.png"
                                                       "neverlogos.svg"
                                                       "svg readme.txt"))
           ("dist/" "share/applications" #:include ("neverball.desktop"
                                                    "neverputt.desktop"))
           ("dist/neverball_16.png"
            "/share/icons/hicolor/16x16/apps/neverball.png")
           ("dist/neverball_24.png"
            "/share/icons/hicolor/24x24/apps/neverball.png")
           ("dist/neverball_32.png"
            "/share/icons/hicolor/32x32/apps/neverball.png")
           ("dist/neverball_48.png"
            "/share/icons/hicolor/48x48/apps/neverball.png")
           ("dist/neverball_64.png"
            "/share/icons/hicolor/64x64/apps/neverball.png")
           ("dist/neverball_128.png"
            "/share/icons/hicolor/128x128/apps/neverball.png")
           ("dist/neverball_256.png"
            "/share/icons/hicolor/256x256/apps/neverball.png")
           ("dist/neverball_512.png"
            "/share/icons/hicolor/512x512/apps/neverball.png")
           ("dist/neverputt_16.png"
            "/share/icons/hicolor/16x16/apps/neverputt.png")
           ("dist/neverputt_24.png"
            "/share/icons/hicolor/24x24/apps/neverputt.png")
           ("dist/neverputt_32.png"
            "/share/icons/hicolor/32x32/apps/neverputt.png")
           ("dist/neverputt_48.png"
            "/share/icons/hicolor/48x48/apps/neverputt.png")
           ("dist/neverputt_64.png"
            "/share/icons/hicolor/64x64/apps/neverputt.png")
           ("dist/neverputt_128.png"
            "/share/icons/hicolor/128x128/apps/neverputt.png")
           ("dist/neverputt_256.png"
            "/share/icons/hicolor/256x256/apps/neverputt.png")
           ("dist/neverputt_512.png"
            "/share/icons/hicolor/512x512/apps/neverputt.png")
           ("dist/" "share/man/man1" #:include ("mapc.1"))
           ("dist/" "share/man/man6" #:include ("neverball.6" "neverputt.6"))
           ("doc/" "share/doc/neverball")
           ("README.md" "share/doc/neverball/"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'install 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (sdl (assoc-ref inputs "sdl")))
                 (invoke "make" "-j" (number->string (parallel-job-count))
                         "--environment-overrides"
                         "CC=gcc" "BUILD=release"
                         (string-append "DATADIR="
                                        out
                                        "/share/games/neverball/data")
                         (string-append "LOCALEDIR=" out "/share/locale")
                         (string-append "SDL_CPPFLAGS=-I"
                                        sdl
                                        "/include/SDL2/")))))
           (add-after 'install 'fix-some-broken-fonts
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/neverball")
                   `("LANG" = ("en_US.utf8")))
                 (wrap-program (string-append out "/bin/neverputt")
                   `("LANG" = ("en_US.utf8")))))))))
      (native-inputs
       `(("gettext" ,gettext-minimal))) ;for msgfmt
      (inputs
       `(("bash-minimal" ,bash-minimal)
         ("libjpeg" ,libjpeg-turbo)
         ("libpng" ,libpng)
         ("libvorbis" ,libvorbis)
         ("physfs" ,physfs)
         ("sdl" ,(sdl-union (list sdl2 sdl2-ttf)))))
      (home-page "https://neverball.org/")
      (synopsis "3D floor-tilting game")
      (description
       "In the grand tradition of Marble Madness and Super Monkey Ball,
Neverball has you guide a rolling ball through dangerous territory.  Balance
on narrow bridges, navigate mazes, ride moving platforms, and dodge pushers
and shovers to get to the goal.  Race against the clock to collect coins to
earn extra balls.  Also included is Neverputt, which is a 3D miniature golf
game.")  ;thanks to Debian for description
      (license license:gpl2+))))

(define-public pokerth
  (package
    (name "pokerth")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pokerth/pokerth/"
                           version "/pokerth-" version ".tar.gz"))
       (sha256
        (base32 "0yi9bj3k8yc1gkwmaf14zbbvvn13n54n1dli8k6j1pkph3p3vjq2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled websocketpp.
           (delete-file-recursively "src/third_party/websocketpp")
           (substitute* "pokerth_lib.pro"
             (("src/third_party/websocketpp")
              ""))))
       (patches (search-patches "pokerth-boost.patch"))))
    (build-system qt-build-system)
    (inputs
     (list boost
           curl
           gsasl
           libgcrypt
           libircclient
           protobuf-2                   ;remove package when no longer needed
           qtbase-5
           (sdl-union (list sdl sdl-mixer))
           sqlite
           tinyxml
           websocketpp
           zlib))
    (arguments
     (list
      #:tests? #f                       ; No test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "." "\\.pro$")
                (("LIB_DIRS =")
                 (string-append "LIB_DIRS = "
                                #$(this-package-input "boost") "/lib")))))
          (add-after 'unpack 'fix-build
            (lambda _
              ;; Fixes for Boost versions >= 1.66.
              (substitute* '("src/net/common/clientthread.cpp"
                             "src/net/serveraccepthelper.h")
                (("boost::asio::socket_base::non_blocking_io command\\(true\\);")
                 "")
                (("newSock->io_control\\(command\\);")
                 "newSock->non_blocking(true);")
                (("acceptedSocket->io_control\\(command\\);")
                 "acceptedSocket->non_blocking(true);"))))
          (replace 'configure
            (lambda _
              (invoke "qmake" "pokerth.pro" "CONFIG+=client"
                      (string-append "PREFIX=" #$output)))))))
    (home-page "https://www.pokerth.net")
    (synopsis "Texas holdem poker game")
    (description
     "With PokerTH you can play the Texas holdem poker game, either against
computer opponents or against real players online.")
    (license license:agpl3+)))

(define-public xblackjack
  (package
    (name "xblackjack")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ibiblio.org/pub/X11/contrib/games/"
                           "xblackjack-" version ".tar.gz"))
       (sha256
        (base32 "05h93rya7zwnx2l58f0a7wkjadymkj4y77clcr2hryhrhhy1vwjx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out (assoc-ref outputs "out")))
               (substitute* "Imakefile"
                 (("EXTRA_LIBRARIES = -lXm \\$\\(DEPLIBS\\) -lbsd")
                  "EXTRA_LIBRARIES = -lXm -lXt -lXmu -lXext -lX11")
                 (("^#define NonStandardInstallTargets NO")
                  "#define NonStandardInstallTargets YES")
                 (("BINDIR = /usr/local/bin")
                  (string-append "BINDIR = " out "/bin"))
                 (("MANDIR = /usr/local/man/cat1")
                  (string-append "MANDIR = " out "/share/man/man1"))
                 (("XAPPLOADDIR = /usr/local/lib/app-defaults")
                  (string-append "XAPPLOADDIR = " out "/lib/X11/app-defaults")))

               (invoke "xmkmf")  ; Generate Makefile.
               (substitute* "Makefile"
                 ((imake) out)
                 (("ETCX11DIR = /etc/X11")
                  (string-append "ETCX11DIR = " out "/etc/X11"))
                 ;; Fix incorrect argument given to gcc. Error message:
                 ;; "gcc: error: DefaultGcc2AMD64Opt: No such file or directory"
                 (("CDEBUGFLAGS = [^\n]*") ""))

               ;; Fix header paths.
               (substitute* '("Draw.c"
                              "Strategy.c")
                 (("^#include <X11/Xm/Xm.h>")
                  "#include <Xm/Xm.h>"))
               (substitute* "Strategy.c"
                 (("^#include <X11/Xm/Label.h>")
                  "#include <Xm/Label.h>"))

               ;; Fix compilation errors.
               (substitute* "Table.c"
                 (("/\\* focus_moved_proc \\*/\tXtInheritFocusMovedProc,") "")
                 (("_XmMoveObject\\(\\(RectObj\\) w, rx, ry\\);")
                  "_XmMoveObject(w, rx, ry);")
                 (("_XmResizeObject\\(\\(RectObj\\) managed->locs[i].w, nw, nh,")
                  "_XmResizeObject(managed->locs[i].w, nw, nh,")))))
         (add-after 'install 'install-man-pages
           (lambda _
             (invoke "make" "install.man"))))
       #:tests? #f))  ; No check target.
    (inputs
     (list lesstif libx11 libxext libxmu libxt))
    (native-inputs
     (list imake))
    (home-page "https://www.ibiblio.org/pub/X11/contrib/games/")
    (synopsis "X11/Motif blackjack game")
    (description
     "Xblackjack is a MOTIF/OLIT based tool constructed to get you ready for
the casino.  It was inspired by a book called \"Beat the Dealer\" by Edward
O. Thorp, Ph.D. of UCLA.  A number of important statistics are maintained
for display, and used by the program to implement Thorp's \"Complete Point
System\" (high-low system).")
    (license (license:x11-style "" "See file headers."))))

(define-public xevil
  ;; This game is old.  Use a maintained fork that builds with modern toolchains
  ;; on modern, 64-bit hardware.
  (let ((commit "9ca85059d5195be0eb15e107de3bb9d1b49e5f99")
        (revision "0"))
    (package
      (name "xevil")
      (version (git-version "2.02" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lvella/xevil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14hsmw9ll2asnp1s0zvniyp31kjw8ynm7vnycg74lpqf28h2rric"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `(,@%default-gnu-imported-modules
                    (srfi srfi-26))
        #:make-flags
        #~(list "SHELL=sh"
                "DEBUG_OPT=-g -DNDEBUG")
        #:tests? #f                     ;no test suite
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'rename-licence-file
              (lambda _ (rename-file "gpl.txt" "COPYING")))
            (add-after 'unpack 'redefine
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "cmn/game.cpp"
                  (("(#define VERSION ).*" _ define)
                   (string-append define "\"" #$version "\"\n")))
                (substitute* "cmn/utils.cpp"
                  (("[^\"]*/(bin/uname)" _ command)
                   (search-input-file inputs command)))
                (substitute* "x11/ui.cpp"
                  ;; Neither DEFAULT_BIG_FONT_NAME nor BACKUP_FONT_NAME are
                  ;; available from most Guix X11 servers, making the game
                  ;; unplayable by default.  Substitute the closest match.
                  (("9x15") "6x13")
                  ;; ‘For fast machines’ need no longer default to False in C21.
                  (("(smoothScroll = )False" _ assign)
                   (string-append assign "True")))))
            (delete 'configure)         ;no configure script
            (replace 'install
              (lambda _
                (with-directory-excursion "x11/REDHAT_LINUX" ;yeah
                  (for-each (cut install-file <>
                                 (string-append #$output "/bin"))
                            (list "xevil" "serverping")))
                (let ((doc (string-append #$output "/share/doc/"
                                          #$name "-" #$version)))
                  (mkdir-p doc)
                  (for-each (lambda (file)
                              (copy-recursively file
                                                (string-append
                                                 doc "/" (basename file))))
                            (list "instructions" "x11/app-defaults"))))))))
      (inputs
       (list coreutils-minimal          ;for uname
             libx11 libxpm))
      ;; The current home page has been ‘subtly’ vandalised with spam and is
      ;; missing a lot of content from this older snapshot.
      (home-page (string-append "https://web.archive.org/web/20060410005819/"
                                "http://www.xevil.com/"))
      (synopsis
       "Third-person, side-scrolling, fast-action, kill-everything game")
      (description
       "XEvil is a violent third-person, side-scrolling, fast-action deathmatch.
You run around a randomly generated two-dimensional map composed of walls,
floors, ladders, doors, and horizontal and vertical elevators.  Your only object
is to explore this world to find weapons and items, killing everything in sight
before they kill you.  You can fight against either computer-controlled enemies
or against other people.")
      (license license:gpl2+))))

(define-public azimuth
  ;; 1.0.3 - May 29, 2019
  ;; Has build errors so build from latest source
  (let ((commit "050f838b35d19ffdc738f33178abaf9d69d834ec")
        (revision "0"))
    (package
      (name "azimuth")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mdsteele/azimuth")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b5grp9ddhrj3n39j1j5vzm3052d4bd09fpz0b011h769vapwi0i"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "test"
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)            ; no configure script
            ;; Build release version instead of debug version.
            (add-after 'unpack 'set-release
              (lambda _
                (setenv "BUILDTYPE" "release")))
            (add-after 'unpack 'fix-build
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* "Makefile"
                  (("-Werror") ""))))
            (add-after 'unpack 'fix-install ; set install directory
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* "Makefile"
                  (("/usr")
                   (assoc-ref outputs "out"))) #t)))))
      (inputs (list sdl2))
      (native-inputs (list pkg-config))
      (home-page "https://mdsteele.games/azimuth/")
      (synopsis "Metroidvania game with vector graphics")
      (description
       "Pilot your ship inside a planet to find and rescue the colonists
trapped inside the Zenith Colony.")
      (license license:gpl3+))))

(define-public cgoban
  (package
    (name "cgoban")
    (version "1.9.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cgoban1/cgoban1/"
                           version "/cgoban-" version ".tar.gz"))
       (sha256
        (base32 "0qlvkiaglqq0izfph3l04mp4rqqqm9ks6rcsrmzrggw9x706z2iv"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxt xorgproto))
    (home-page "https://cgoban1.sourceforge.net/")
    (synopsis "Go client for X11")
    (description "Provides a large set of Go-related services for X11:
@itemize
@item Local games with precise implementation of the Chinese and Japanese rulesets
@item Edition and visualization of SGF files
@item Connection to the NNGS or IGS Go servers
@item Bridge to Go modem protocol, allowing to play against Go modem-capable AIs
such as GnuGo.
@end itemize")
    (license license:gpl2+)))

(define-public passage
  (package
    (name "passage")
    (version "4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/hcsoftware/Passage/v"
                           version "/Passage_v" version "_UnixSource.tar.gz"))
       (sha256
        (base32 "02ky4a4xdjvr71r58339jjrjyz76b5skcnbq4f8707mrln9vhby3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false                  ; there are none
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "gamma256/gameSource")
             (system "cat Makefile.GnuLinux Makefile.all > Makefile")))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "sdl") "/include/SDL:"
                      (or (getenv "CPATH") "")))
             (let* ((out (assoc-ref outputs "out"))
                    (assets (string-append out "/share/passage")))
               (substitute* "common.cpp"
                 (("readTGA\\( \"graphics\"")
                  (format #false "readTGA(\"~a/graphics\"" assets)))
               (substitute* "musicPlayer.cpp"
                 (("readTGA\\( \"music\"")
                  (format #false "readTGA(\"~a/music\"" assets))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (assets (string-append out "/share/passage/")))
               (install-file "Passage" bin)
               (install-file "../documentation/Readme.txt" assets)
               (copy-recursively "graphics" (string-append assets "graphics"))
               (copy-recursively "music" (string-append assets "music"))
               (copy-recursively "settings" (string-append assets "settings"))))))))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-mixer)))))
    (native-inputs
     (list imagemagick))
    (home-page "https://hcsoftware.sourceforge.net/passage/")
    (synopsis "Memento mori game")
    (description
     "Passage is meant to be a memento mori game.  It presents an entire life,
from young adulthood through old age and death, in the span of five minutes.
Of course, it's a game, not a painting or a film, so the choices that you make
as the player are crucial.  There's no ``right'' way to play Passage, just as
there's no right way to interpret it.")
    (license license:public-domain)))

(define-public paperview
  (let ((commit "9f8538eb6734c76877b878b8f1e52587f2ae19e6")
        (revision "1"))
    (package
      (name "paperview")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/glouw/paperview")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09sb9sg44fvkgfdyycrm1ndpx7cnkrglxhci41y8f3gpagnvi7jk"))))
      (build-system gnu-build-system)
      (inputs
       (list sdl2))
      (arguments
       '(#:tests? #f ;no tests
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (home-page "https://github.com/glouw/paperview/")
      (synopsis "High performance X11 animated wallpaper setter")
      (description "High performance animated desktop background setter for
X11 that won't set your CPU on fire, drain your laptop battery, or lower video
game FPS.")
      (license license:unlicense))))

(define-public curseofwar
  (package
    (name "curseofwar")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a-nikolaev/curseofwar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wd71wdnj9izg5d95m81yx3684g4zdi7fsy0j5wwnbd9j34ilz1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs (list ncurses))
    (home-page "https://a-nikolaev.github.io/curseofwar/")
    (synopsis "Fast-paced action strategy game")
    (description "Curse of War is a fast-paced action strategy game originally
implemented using ncurses user interface.  An SDL graphical version is also
available.")
    (license license:gpl3+)))

(define-public devours
  (let ((commit "d50e745aa14aa48f7555ae12eb3d1000de1cc150")
        (revision "0"))
  (package
    (name "devours")
    (version (git-version "3" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://jxself.org/git/devours.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ksl6mh76jfx64rmasz2571f88ws45vby2977srhgkh355zp3lzn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure
          (replace 'build
            (lambda _
              (invoke "inform"
                      (string-append "+include_path="
                                     #$(this-package-native-input "informlib")
                                     "/lib")
                      "devours.inf")))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Create standalone executable.
              (let* ((bash (search-input-file inputs "/bin/bash"))
                     (share (string-append #$output "/share"))
                     (scummvm (search-input-file inputs "/bin/scummvm"))
                     (bin (string-append #$output "/bin"))
                     (executable (string-append bin "/devours")))
                (mkdir-p share)
                (copy-file "devours.z5" (string-append share "/devours.z5"))
                (mkdir-p bin)
                (with-output-to-file executable
                  (lambda ()
                    (format #t "#!~a~%" bash)
                    (format #t
                            "exec ~a --path=~a glk:zcode~%"
                            scummvm share)))
                (chmod executable #o755))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (let* ((apps (string-append #$output "/share/applications"))
                     (share (string-append #$output "")))
                (mkdir-p apps)
                (make-desktop-entry-file
                 (string-append apps "/devours.desktop")
                 #:name "All Things Devours"
                 #:generic-name "All Things Devours"
                 #:exec (string-append #$output "/bin/devours")
                 #:categories '("AdventureGame" "Game" "RolePlaying")
                 #:keywords '("game" "adventure" "sci-fi")
                 #:comment '((#f "Sci-fi text adventure game")))))))))
    (inputs
     (list bash scummvm))
    (native-inputs
     (list inform informlib))
    (synopsis "All Things Devours")
    (description
     "All Things Devours is a short piece of sci-fi interactive fiction,
leaning strongly towards the text-adventure end of the spectrum.
Any move you make may put things into an unwinnable state.  You are therefore
encouraged to save frequently, and also to realise that you will probably have
to start over several times to find the most satisfactory ending.")
    (home-page "https://jxself.org/git/devours.git")
    (license license:agpl3+))))

(define-public schiffbruch
  ;; There haven't been any releases for several years, so I've taken the most
  ;; recent commit from the master branch that didn't fail to build (the last
  ;; commit gave me a compile error).
  (let ((commit "e41916d15d87749c82c5005cbb42d1bb079b43d9"))
    (package
      (name "schiffbruch")
      (version (git-version "1.2.1" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sandsmark/Schiffbruch")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lg3rqacrapf6c4sxi12cm9bmg43mlbclway1zxcm848pi1xkzwv"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                              ; no tests
         #:build-type "Release"))
      (inputs
       (list sfml))
      (home-page "https://github.com/sandsmark/Schiffbruch/")
      (synopsis "Pixelart survival game")
      (description
       "Schiffbruch is a mix of building, strategy and adventure and gets played
with a two-dimensional view.  The game deals with the consequences of a ship
wreckage.  You're stranded on a desert island and have to survive.  In order to
do so you need to explore the island, find food, build a shelter and try to
get attention, so you get found.")
      (license license:cc-by4.0))))

(define-public sdl-jstest
  (package
    (name "sdl-jstest")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Grumbel/sdl-jstest")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11qp4gkbb11n3wx74128fj56radgsvkj7nxhbh55rd3xad1hckh3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; The test suite uses appstream-utils to validate the appdata.xml file,
      ;; fails with "url-not-found".
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-gamecontroller-db
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (copy-file
               (search-input-file (or native-inputs inputs)
                                  "share/sdl2/gamecontrollerdb.txt")
               "external/sdl_gamecontrollerdb/gamecontrollerdb.txt"))))))
    (native-inputs
     (list pkg-config sdl2-gamecontrollerdb tinycmmc))
    (inputs (list ncurses sdl sdl2))
    (home-page "https://github.com/Grumbel/sdl-jstest")
    (synopsis "SDL Joystick Tester")
    (description "The @command{sdl-jstest} and @command{sdl2-jstest} commands
can list the available joystick controllers as found by the SDL or SDL2
libraries, respectively.  It can show the available axes, buttons, hats and
balls of a chosen controller, and can display the controller actions in real
time in a visual fashion.")
    (license license:gpl3+)))

(define-public sdlpop
  (package
    (name "sdlpop")
    (version "1.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NagyD/SDLPoP")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yy5r1r0hv0xggk8qd8bwk2zy7abpv89nikq4flqgi53fc5q9xl7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests provided
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'prepare-build
           ;; Set correct environment for SDL.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl")
                                    "/include/SDL2:"
                                    (or (getenv "CPATH") "")))))
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (opt (string-append out "/opt/sdlpop"))
                    (app (string-append out "/usr/share/applications"))
                    (template "src/SDLPoP.desktop.template"))
               (chdir "..")
               (install-file "prince" bin)
               (substitute* template (("\\$ROOT") out))
               (substitute* "src/seg009.c"
                 (("g_argv[0]") (string-append "\"" out "\"")))
               (install-file template app)
               (rename-file (string-append app "/SDLPoP.desktop.template")
                            (string-append app "/SDLPoP.desktop"))
               (install-file "SDLPoP.ini" opt)
               (copy-recursively "data" (string-append bin "/data"))
               (copy-recursively "doc" opt)
               (copy-recursively "mods" opt)))))))
    (native-inputs (list pkg-config))
    (inputs `(("sdl" ,(sdl-union (list sdl2
                                       sdl2-image
                                       sdl2-mixer)))))
    (synopsis "Port of Prince of Persia game")
    (description "This package provides port of Prince of Persia, based on the
disassembly of the DOS version, extended with new features.")
    (home-page "https://github.com/NagyD/SDLPoP")
    (license license:gpl3+)))

(define-public fheroes2
  (package
    (name "fheroes2")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ihhub/fheroes2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zi8p8932pnmgjqm08l2ql5lwdrl9bcsm8bzf66hciw85l6dlbi3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags '("FHEROES2_STRICT_COMPILATION=1"
                      "RELEASE=1")))
    (native-inputs
     (list gettext-minimal))
    (inputs
     (list libpng
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
           zlib))
    (home-page "https://ihhub.github.io/fheroes2/")
    (synopsis "Turn-based strategy game engine")
    (description "@code{fheroes2} is an implementation of Heroes of Might and
Magic II (aka HOMM2) game engine.  It requires assets and game resources to
play; it will look for them at @file{~/.local/share/fheroes2} folder.")
    (license license:gpl2)))

(define-public vcmi
  (package
    (name "vcmi")
    (version "1.6.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vcmi/vcmi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sdcaxi9npjz7y6mxpbaz39idhkh05dlj2fz8f8xan21lhailvz4"))
              (patches (search-patches "vcmi-disable-privacy-breach.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DFORCE_BUNDLED_FL=OFF" "-DENABLE_INNOEXTRACT=OFF")
           ;; Test suites do not seem well supported upstream and are disabled by default.
           ;; Pass -DENABLE_TEST to configure to enable.
           #:tests? #f))
    (native-inputs
     (list boost
           ffmpeg
           fuzzylite
           ;; googletest ; needed for tests, but tests are disabled
           libxkbcommon
           luajit
           minizip
           pkg-config
           python
           qtbase
           qttools
           sdl2
           sdl2-mixer
           sdl2-image
           sdl2-ttf
           tbb
           vulkan-headers
           zlib))
    (home-page "https://vcmi.eu/")
    (synopsis "Turn-based strategy game engine")
    (description
     "@code{vcmi} is an implementation of the Heroes of Might and
Magic III game engine.  It requires assets and game resources to
play; it will look for them at @file{~/.local/share/vcmi} folder.")
    (license license:gpl2)))

(define-public apricots
  (package
    (name "apricots")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moggers87/apricots")
             (commit (string-append "v" version))))
       (sha256
        (base32 "01mqdybmn5rp8ifx619bx0pki9ryj5cvv2iwpsnn8ngggd6smh9x"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; The test suite doesn't test the built game, but merely runs cppcheck &
     ;; clang-format.  Useful for the maintainers; not for distributions.
     (list #:tests? #f))
    (native-inputs (list autoconf       ;autom4te used in ./bootstrap
                         automake))     ;aclocal used in ./bootstrap
    (inputs (list alure openal sdl2))
    (home-page "https://github.com/moggers87/apricots")
    (synopsis "Arcade airplane game")
    (description "@code{apricots} is a game where you fly a little plane
around the screen and shoot things and drop bombs on enemy targets.  It's
meant to be quick and fun.")
    (license license:gpl2+)))

(define-public liquidwar6
  (package
    (name "liquidwar6")
    (version "0.6.3902")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/liquidwar6/" "liquidwar6-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1976nnl83d8wspjhb5d5ivdvdxgb8lp34wp54jal60z4zad581fn"))))
    (native-inputs (list doxygen))
    (inputs (list guile-2.0
                  zlib
                  expat
                  sqlite
                  ncurses
                  readline
                  curl
                  python-2
                  libxslt
                  perl
                  graphviz
                  glu
                  libcaca
                  (sdl-union (list sdl sdl-image sdl-ttf sdl-mixer))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-allinone" "CFLAGS=-Wno-error -O2 -g"
                   (string-append "CPPFLAGS=" "-I"
                                  #$(this-package-input "sdl-union")
                                  "/include/SDL"))))
    (synopsis "Liquid War 6 is a unique multiplayer wargame")
    (description
     "Liquid War 6 is a unique multiplayer war game.  Your army is a blob of
liquid and you have to try and eat your opponents.  Rules are very simple yet
original, they have been invented by Thomas Colcombet.")
    (home-page "https://www.gnu.org/software/liquidwar6/")
    (license license:gpl3+)))

(define-public plunder
  (let ((commit "026ded7083df5134bdf05b1ec7e5a0099ac9b9d2")
        (revision "1"))
    (package
      (name "plunder")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jappeace/plunder")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0m0v8x6q9iq4zihwmysbxjwkq18nar6xhq4g18p2g8c6azj2mgd6"))))
      (build-system haskell-build-system)
      (inputs (list ghc-monadrandom
                    ghc-quickcheck
                    ghc-file-embed
                    ghc-generic-lens
                    ghc-lens
                    ghc-random
                    ghc-reflex
                    ghc-reflex-sdl2
                    ghc-sdl2
                    ghc-sdl2-gfx
                    ghc-sdl2-image
                    ghc-sdl2-ttf
                    ghc-vector
                    ghc-witherable))
      (native-inputs (list ghc-hspec ghc-hspec-core hspec-discover))
      (home-page "https://github.com/jappeace/plunder")
      (synopsis "Game about looting a hexagonal-tile world")
      (description
       "This package provides a work-in-progress game where you control a
Viking and your objective is to loot all of the occupied hexagonal tiles in
the map.")
      (license license:expat))))

(define-public freerct
  (package
    (name "freerct")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FreeRCT/FreeRCT")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1szwy2cq4ffp4yxm9pp9vdyia0i5nz0wnppdd1xb9w7v3wa4mywi"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list flex bison))
    (inputs (list libpng sdl2 sdl2-ttf))
    (home-page "https://freerct.net/")
    (synopsis "Theme park management simulation game")
    (description
     "FreeRCT is a game that captures the look and feel of the popular games
RollerCoaster Tycoon 1 and 2, graphics- and gameplay-wise.

In this game, you play as a manager of a theme park, allowing you to make a
park of your dreams.  The list of responsibilities includes managing staff,
finances, landscaping, and most importantly: rides.  Good managers follow the
principle of prioritizing the guests' happiness with a well-maintained park.
Should they go unwise, a theme park plunge into chaos with vandalizing guests
and unsafe rides.  Which path will you take?")
    (license license:gpl2)))

(define-public ultrastar-deluxe
  (package
    (name "ultrastar-deluxe")
    (version "2024.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/UltraStar-Deluxe/USDX.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16q6b5nnjx5baq4m30ys47970kjgp06xihyd6qyb08s0yk2f54jz"))
              (patches (search-patches "ultrastar-deluxe-no-freesans.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Remove Windows binaries.
                   (for-each delete-file (find-files "game" "\\.dll$"))
                   ;; Remove font blobs.
                   (let ((font-directories
                          (list "DejaVu" "FreeSans" "NotoSans"
                                "wqy-microhei")))
                     (for-each
                      (lambda (d) (delete-file-recursively
                              (string-append "game/fonts/" d)))
                      font-directories))))))
    (build-system gnu-build-system)
    (arguments
      (list
       #:tests? #f ; No tests.
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-configure
             (lambda* (#:key inputs configure-flags outputs #:allow-other-keys)
               (define (where inputs file)
                 (dirname (search-input-file inputs file)))
               ;; The configure script looks for lua$version, but we
               ;; provide lua-$version.
               (substitute* "configure.ac"
                 (("lua\\$i") "lua-$i"))
               ;; fpc does not pass -lfoo to the linker, but uses its own
               ;; linker script, which references libs.  Pass the libraries
               ;; listed in that linker script, so our custom linker adds
               ;; a correct rpath.
               (substitute* "src/Makefile.in"
                 (("linkflags\\s+:= ")
                  (string-append
                   "linkflags := -lpthread -lsqlite3 -lSDL2"
                   " -lSDL2_image -ldl "
                   " -lz -lfreetype -lportaudio -lavcodec"
                   " -lavformat -lavutil -lswresample"
                   " -lswscale -llua -ldl -lX11 -lportmidi"
                   " -L" (where inputs "lib/libz.so")
                   " -L" (where inputs "lib/libX11.so")
                   " -L" (where inputs "lib/libportmidi.so"))))))
           (add-after 'install 'font-paths
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* (string-append
                             (assoc-ref outputs "out")
                             "/share/ultrastardx/fonts/fonts.ini")
                 (("=NotoSans/") (string-append "=" #$font-google-noto:ttf
                                                "/share/fonts/truetype/"))
                 (("=DejaVu/") (string-append "=" #$font-dejavu
                                              "/share/fonts/truetype/"))))))))
    (inputs (list ffmpeg
                  font-dejavu
                  (list font-google-noto "ttf")
                  ; Not needed, since we don’t have freesans.
                  ;font-wqy-microhei
                  freetype
                  libx11
                  lua
                  portaudio
                  portmidi
                  sdl2
                  sdl2-image
                  sqlite
                  zlib))
    (native-inputs (list pkg-config fpc autoconf automake))
    (synopsis "Karaoke game")
    (description
     "UltraStar Deluxe (USDX) is a karaoke game.  It allows up to six players
to sing along with music using microphones in order to score points, depending
on the pitch of the voice and the rhythm of singing.")
    (home-page "https://usdx.eu/")
    (license license:gpl2+)))

(define-public xmahjongg
  (package
    (name "xmahjongg")
    (version "3.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.lcdf.org/xmahjongg/xmahjongg-"
                           version ".tar.gz"))
       (sha256
        (base32 "1kzz8y34q7wibcrmfb3p9rrz88qriz4slxpf1yrrfny23il66g94"))))
    (build-system gnu-build-system)
    (inputs (list libx11))
    (home-page "http://www.lcdf.org/xmahjongg/")
    (synopsis "Solitaire Mah Jongg game")
    (description
     "Xmahjongg is a simple solitaire game.  The object is to remove all Mah
Jongg tiles from the playing field by taking one matching pair at a time.")
    (license license:gpl2+)))

(define-public steam-devices-udev-rules
  ;; Last release from 2019-04-10
  (let ((commit "13443480a64fe8f10676606bd57da6de89f8ccb1")
        (revision "1"))
    (package
      (name "steam-devices-udev-rules")
      (version (git-version "1.0.0.61" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ValveSoftware/steam-devices")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0i086gmnk93q76sw1laa9br6b7zj2r6nrrw7d64y4q9wcrlxw2bi"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("./" "lib/udev/rules.d"
                           #:include-regexp ("rules$")))
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-paths
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "60-steam-input.rules"
                          (("/bin/sh")
                           (search-input-file inputs "/bin/sh"))
                          (("udevadm")
                           (search-input-file inputs "/bin/udevadm"))))))))
      (inputs (list eudev))
      (home-page "https://github.com/ValveSoftware/steam-devices")
      (synopsis "udev rules for game controllers and virtual reality devices")
      (description
       "This package provides a set of udev rules for game controllers and
virtual reality devices.")
      (license license:expat))))

(define-public gemrb
  (package
    (name "gemrb")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gemrb/gemrb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16pp9vw717pk9q8q3asxk4j64rmywbnpw91cr3qanwnmdi5p5gj4"))
       ;; Remove the patch in the next version, as commit cca8e71 fixes this
       (patches (search-patches
                 "gemrb-remove-ifdef-and-externalize-path-setting-to-cmake.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:cmake cmake-next
      #:configure-flags
      #~(list "-DUSE_TESTS=ON" "-DOPENGL_BACKEND=OpenGL")))
    (native-inputs (list python-3.10 glibc-locales googletest))
    (inputs (list freetype
                  libiconv
                  libpng
                  libvorbis
                  openal
                  sdl2
                  sdl2-mixer
                  vlc
                  zlib))
    (home-page "https://gemrb.org/")
    (synopsis "Portable implementation of Bioware's Infinity Engine")
    (description
     "GemRB (Game Engine Made with preRendered Background) is a portable
reimplementation of the Infinity Engine that underpinned Baldur's Gate,
Icewind Dale and Planescape: Torment.  It sports a cleaner design, greater
extensibility and several innovations.")
    (license license:gpl2+)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
