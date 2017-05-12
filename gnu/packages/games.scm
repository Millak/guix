;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014, 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016, 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2016 Albin Söderqvist <albin@fripost.org>
;;; Copyright © 2016, 2017 Kei Kebreau <kei@openmailbox.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Steve Webber <webber.sl@gmail.com>
;;; Copyright © 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@openmailbox.org>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages wxwidgets)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial))

(define-public freedoom
  (package
   (name "freedoom")
   (version "0.11.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/" name "/" name
                                "/archive/v" version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "0b9k61f97spivi75f76zwwg8a3bgc6iil2hidqfj8s50lhqggwbb"))))
   (build-system gnu-build-system)
   (arguments
    '(#:make-flags `(,(string-append "prefix=" (assoc-ref %outputs "out")))
      #:parallel-build? #f
      #:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (add-before 'unpack 'no (lambda _ #t))
        (replace 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((dejavu (assoc-ref inputs "font-dejavu"))
                          (freedoom (assoc-ref outputs "out"))
                          (wad-dir (string-append freedoom "/share/games/doom")))
                     ;; Replace the font-searching function in a shell
                     ;; script with a direct path to the required font.
                     ;; This is necessary because ImageMagick can only find the
                     ;; most basic fonts while in the build environment.
                     (substitute* "graphics/titlepic/create_caption"
                       (("font=\\$\\(find_font.*$")
                        (string-append
                         "font=" dejavu
                         "/share/fonts/truetype/DejaVuSansCondensed-Bold.ttf\n")))
                     ;; Make icon creation reproducible.
                     (substitute* "dist/Makefile"
                       (("freedm.png")
                        "-define png:exclude-chunks=date freedm.png")
                       (("freedoom1.png")
                        "-define png:exclude-chunks=date freedoom1.png")
                       (("freedoom2.png")
                        "-define png:exclude-chunks=date freedoom2.png"))
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
    `(("asciidoc" ,asciidoc)
      ("deutex" ,deutex)
      ("font-dejavu" ,font-dejavu)
      ("imagemagick" ,imagemagick)
      ("python" ,python-2)))
   (inputs
    `(("prboom-plus" ,prboom-plus)))
   (home-page "https://freedoom.github.io/")
   (synopsis "Free content game based on the Doom engine")
   (description
    "The Freedoom project aims to create a complete free content first person
shooter game.  Freedoom by itself is just the raw material for a game: it must
be paired with a compatible game engine (such as @code{prboom-plus}) to be
played.  Freedoom complements the Doom engine with free levels, artwork, sound
effects and music to make a completely free game.")
   (license license:bsd-3)))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.gnubg.org/media/sources/gnubg-release-"
                           version ".000-sources." "tar.gz"))
       (sha256
        (base32
         "015mvjk2iw1cg1kxwxfnvp2rxb9cylf6yc39i30fdy414k07zkky"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("readline" ,readline)
              ("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("gtkglext" ,gtkglext)
              ("sqlite" ,sqlite)
              ("libcanberra" ,libcanberra)))
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://gnubg.org")
    (synopsis "Backgammon game")
    (description "The GNU backgammon application can be used for playing, analyzing and
teaching the game.  It has an advanced evaluation engine based on artificial
neural networks suitable for both beginners and advanced players.  In
addition to a command-line interface, it also features an attractive, 3D
representation of the playing board.")
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
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libx11" ,libx11)
              ("guile" ,guile-2.0)
              ("gtkglext" ,gtkglext)))
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
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/gnushogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0a9bsl2nbnb138lq0h14jfc5xvz7hpb2bcsj4mjn6g1hcsl4ik0y"))))
    (arguments `(#:tests? #f)) ;; No check target.
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "The game of Shogi (Japanese chess)")
    (description  "GNU Shogi is a program that plays the game Shogi (Japanese
Chess).  It is similar to standard chess but this variant is far more complicated.")
    (license license:gpl3+)))

(define-public prboom-plus
  (package
   (name "prboom-plus")
   (version "2.5.1.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                version "/" name "-" version ".tar.gz"))
            (sha256
             (base32
              "151v6nign86m1a2vqz27krsccpc9m4d1jax4y43v2fa82wfj9qp0"))
            (modules '((guix build utils)))
            (snippet
             '(substitute* "src/version.c"
                           (("__DATE__") "")
                           (("__TIME__") "")))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags '("--disable-cpu-opt")
      #:make-flags `(,(string-append "gamesdir="
                                     (assoc-ref %outputs "out") "/bin"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'set-paths 'set-sdl'paths
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "CPATH"
                    (string-append (assoc-ref inputs "sdl-union")
                                   "/include/SDL"))
            #t)))))
   (inputs
    `(("fluidsynth" ,fluidsynth)
      ("glu" ,glu)
      ("libmad" ,libmad)
      ("libpng" ,libpng)
      ("libvorbis" ,libvorbis)
      ("pcre" ,pcre)
      ("portmidi" ,portmidi)
      ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net)))))
   (home-page "http://prboom-plus.sourceforge.net/")
   (synopsis "Version of the classic 3D shoot'em'up game Doom")
   (description
    "PrBoom+ is a Doom source port developed from the original PrBoom project.")
   (license license:gpl2+)))

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
     `(("libxaw" ,libxaw)
       ("libxt" ,libxt)))
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "User interface for gnushogi")
    (description  "A graphical user interface for the package @code{gnushogi}.")
    ;; Contains a copy of GPLv3 but the licence notices simply
    ;; state "GNU General Public Licence" without specifying a version.
    (license license:gpl1+)))

(define-public abbaye
  (package
    (name "abbaye")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nevat/abbayedesmorts-gpl/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a67b0hq6271dd7pvwndjq29cwn2n8gawwz17xafa3k1hrhf8vw3"))
       (modules '((guix build utils)))
       (snippet
        ;; Unbundle fonts.
        '(delete-file-recursively "fonts"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append (assoc-ref inputs "sdl-union")
                                             "/include/SDL"))))
                  (add-after 'patch-source-shebangs 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Replace /usr with package output directory.
                      (substitute* "Makefile"
                        (("/usr") (assoc-ref outputs "out")))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        ;; Create directories that the makefile assumes exist.
                        (mkdir-p (string-append prefix "/bin"))
                        (mkdir-p (string-append prefix "/share/applications"))
                        (mkdir-p (string-append prefix "/share/pixmaps")))))
                  ;; No configure script.
                  (delete 'configure))
       #:tests? #f)) ;; No check target.
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer)))))
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
    (version "4.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://rephial.org/downloads/4.0/"
                           "angband-" version ".tar.gz"))
       (sha256
        (base32
         "0lpq2kms7hp421vrasx2bkkn9w08kr581ldwik3v0hlq6h7rlxhd"))
       (modules '((guix build utils)))
       (snippet
        ;; So, some of the sounds/graphics/tilesets are under different
        ;; licenses... some of them even nonfree!  This is a console-only
        ;; version of this package so we just remove them.
        ;; In the future, if someone tries to make a graphical variant of
        ;; this package, they can deal with that mess themselves. :)
        '(begin
           (for-each
            (lambda (subdir)
              (let ((lib-subdir (string-append "lib/" subdir)))
                (delete-file-recursively lib-subdir)))
            '("fonts" "icons" "sounds" "tiles"))
           (substitute* "lib/Makefile"
             ;; And don't try to invoke makefiles in the directories we removed
             (("gamedata customize help screens fonts tiles sounds icons user")
              "gamedata customize help screens user"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                 ;no check target
       #:configure-flags (list (string-append "--bindir=" %output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen.sh
           (lambda _
             (substitute* "acinclude.m4"
               (("ncursesw5-config") "ncursesw6-config"))
             (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://rephial.org/")
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
       (method url-fetch)
       (uri (string-append "https://github.com/Pingus/pingus/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0r9v6as5vi7skvvy7b0fcaimhdlzmik64pyy68plgljhsghqkkf4"))
       (patches (search-patches "pingus-sdl-libs-config.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("scons" ,scons)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libpng" ,libpng)
              ("boost" ,boost)))
    (arguments
     '(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (zero? (system* "make" "install"
                            (string-append "PREFIX="
                                           (assoc-ref outputs "out")))))))))
    (home-page "http://pingus.seul.org/welcome.html")
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
       (uri (string-append "http://www.hyperrealm.com/" name "/"
                           name  "-" version  ".tar.gz"))
       (sha256
        (base32 "19nc5vq4bnkjvhk8srqddzhcs93jyvpm9r6lzjzwc1mgf08yg0a6"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/talkfilters/")
    (synopsis "Convert English text to humorous dialects")
    (description "The GNU Talk Filters are programs that convert English text
into stereotyped or otherwise humorous dialects.  The filters are provided as
a C library, so they can easily be integrated into other programs.")
    (license license:gpl2+)))

(define-public cmatrix
  (package
    (name "cmatrix")
    (version "1.2a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.asty.org/cmatrix/dist/cmatrix-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0k06fw2n8nzp1pcdynhajp5prba03gfgsbj91bknyjr5xb5fd9hz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This old ‘configure’ script doesn't support
             ;; variables passed as arguments.
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)))))))))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license license:gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1vw2w3jwnmn44d5vsw47f8y70xvxcsz9m5msq9fgqlzjch15qhiw"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/chess/")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (license license:gpl3+)))

(define freedink-engine
  (package
    (name "freedink-engine")
    (version "108.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "08c51imfjfcydm7h0va09z8qfw5nc837bi2x754ni2z737hb5kw2"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--disable-embedded-resources")))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)
              ("sdl-gfx" ,sdl-gfx)
              ("fontconfig" ,fontconfig)
              ("check" ,check)))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Twisted adventures of young pig farmer Dink Smallwood")
    (description
     "GNU FreeDink is a free and portable re-implementation of the engine
for the role-playing game Dink Smallwood.  It supports not only the original
game data files but it also supports user-produced game mods or \"D-Mods\".
To that extent, it also includes a front-end for managing all of your D-Mods.")
    (license license:gpl3+)))

(define freedink-data
  (package
    (name "freedink-data")
    (version "1.08.20170401")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1zx7qywibhznj7bnz217404scr8dfh0xj24xjihnda5iapzz7lz8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check))               ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

;; TODO: Add freedink-dfarc when there's a wxWidgets package.

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
                         (chmod port #o777)))))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

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
    (inputs
     `(("gtk+" ,gtk+-2)
       ("librsvg" ,librsvg)))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/xboard/")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license license:gpl3+)))


(define-public xboing
  (package
    (name "xboing")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.techrescue.org/xboing/xboing"
                           version ".tar.gz"))
       (sha256
        (base32 "16m2si8wmshxpifk861vhpqviqxgcg8bxj6wfw8hpnm4r2w9q0b7"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)

             (substitute* "Imakefile"
               (("XPMINCLUDE[\t ]*= -I/usr/X11/include/X11")
                (string-append "XPMINCLUDE = -I"
                               (assoc-ref %build-inputs "libxpm")
                               "/include/X11")))

             (substitute* "Imakefile"
               (("XBOING_DIR = \\.") "XBOING_DIR=$(PROJECTROOT)"))

             ;; FIXME: HIGH_SCORE_FILE should be set to somewhere writeable

             (zero? (system* "xmkmf" "-a"
                             (string-append "-DProjectRoot="
                                            (assoc-ref outputs "out"))))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (and
             (zero? (system* "make" "install.man"))
             (zero? (system* "make" "install"))))))))
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxpm" ,libxpm)))
    (native-inputs `(("imake" ,imake)
                     ("inetutils" ,inetutils)
                     ("makedepend" ,makedepend)))
    (build-system gnu-build-system)
    (home-page "http://www.techrescue.org/xboing")
    (synopsis "Ball and paddle game")
    (description "XBoing is a blockout type game where you have a paddle which
you control to bounce a ball around the game zone destroying blocks with a
proton ball.  Each block carries a different point value.  The more blocks you
destroy, the better your score.  The person with the highest score wins.")
    (license (license:x11-style "file://COPYING"
                                "Very similar to the X11 licence."))))

(define-public gtypist
  (package
    (name "gtypist")
    (version "2.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gtypist/gtypist-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xzrkkmj0b1dw3yr0m9hml2y634cc4h61im6zwcq57s7285z8fn1"))
              (modules '((guix build utils)))
              (snippet
               ;; We do not provide `ncurses.h' within an `ncursesw'
               ;; sub-directory, so patch the source accordingly.  See
               ;; <http://bugs.gnu.org/19018>.
               '(for-each (lambda (file)
                            (substitute* file
                              (("ncursesw/ncurses.h")
                               "ncurses.h")))
                          (find-files "." "configure$|\\.c$")))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)))
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
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0cz4z4dwrv5ypl19ll67wl6jjpy5k6ly4vr042w4br88qq5jhazl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("INSTALL_DIR = /usr/local/lib")
                  (string-append "INSTALL_DIR = " out "/lib")))
               ;; The Makefile assumes these directories exist.
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/include")))))
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (zero? (system* "unzip" source))
                  ;; The actual source is buried a few directories deep.
                  (chdir (string-append "irrlicht-" ,version
                                        "/source/Irrlicht/")))))
         (delete 'configure))           ; no configure script
       #:tests? #f                      ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("mesa" ,mesa)
       ("glu" ,glu)))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "http://irrlicht.sourceforge.net/")
    (license license:zlib)))

(define-public mars
  ;; The latest release on SourceForge relies on an unreleased version of SFML
  ;; with a different API, so we take the latest version from the official
  ;; repository on Github.
  (let ((commit   "c855d044094a1d92317e38935d81ba938946132e")
        (revision "1"))
    (package
      (name "mars")
      (version (string-append "0.7.5." revision "." (string-take commit 7) ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelaui/M.A.R.S..git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1r4c5gap1z2zsv4yjd34qriqkxaq4lb4rykapyzkkdf4g36lc3nh"))
                (patches (search-patches "mars-sfml-2.3.patch"
                                         "mars-install.patch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f        ; There are no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-install-path
            (lambda _
              (substitute* "src/CMakeLists.txt"
                (("\\$\\{CMAKE_INSTALL_PREFIX\\}/games")
                 "${CMAKE_INSTALL_PREFIX}/bin"))
              #t))
           (add-after 'unpack 'fix-data-path
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/System/settings.cpp"
                (("C_dataPath = \"./data/\";")
                 (string-append "C_dataPath = \""
                                (assoc-ref outputs "out")
                                "/share/games/marsshooter/\";")))
              #t)))))
      (inputs
       `(("mesa" ,mesa)
         ("fribidi" ,fribidi)
         ("taglib" ,taglib)
         ("sfml" ,sfml)))
      (home-page "http://marsshooter.org")
      (synopsis "2D space shooter")
      (description
       "M.A.R.S. is a 2D space shooter with pretty visual effects and
attractive physics.  Players can battle each other or computer controlled
enemies in different game modes such as space ball, death match, team death
match, cannon keep, and grave-itation pit.")
      (license license:gpl3+))))

(define minetest-data
  (package
    (name "minetest-data")
    (version "0.4.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest_game/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12kxklcd1b5n0f369zf3xxhvkrizxj4d9xv0dh8crfqiymaym0zm"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,(@ (gnu packages compression) gzip))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tar (string-append (assoc-ref %build-inputs "tar")
                                             "/bin/tar"))
                         (install-dir (string-append
                                       %output
                                       "/share/minetest/games/minetest_game"))
                         (path (string-append (assoc-ref %build-inputs
                                                         "gzip")
                                              "/bin")))
                     (setenv "PATH" path)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))
                     (chdir (string-append "minetest_game-" ,version))
                     (mkdir-p install-dir)
                     (copy-recursively "." install-dir)))))
    (synopsis "Main game data for the Minetest game engine")
    (description
     "Game data for the Minetest infinite-world block sandox game.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

(define-public minetest
  (package
    (name "minetest")
    (version "0.4.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ag3jcj7kpq3ph12zirk1c0mj9i0g50wmw932f8gi11liq8yd12n"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
         (list "-DRUN_IN_PLACE=0"
               "-DENABLE_FREETYPE=1"
               "-DENABLE_GETTEXT=1"
               (string-append "-DIRRLICHT_INCLUDE_DIR="
                              (assoc-ref %build-inputs "irrlicht")
                              "/include/irrlicht")
               (string-append "-DCURL_INCLUDE_DIR="
                              (assoc-ref %build-inputs "curl")
                              "/include/curl"))
       #:tests? #f)) ; no check target
    (native-search-paths
     (list (search-path-specification
            (variable "MINETEST_SUBGAME_PATH")
            (files '("share/minetest/games")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("irrlicht" ,irrlicht)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("freetype" ,(@ (gnu packages fontutils) freetype))
       ("curl" ,curl)
       ("luajit" ,luajit)
       ("gettext" ,gettext-minimal)
       ("sqlite" ,sqlite)))
    (propagated-inputs
     `(("minetest-data" ,minetest-data)))
    (synopsis "Infinite-world block sandbox game")
    (description
     "Minetest is a sandbox construction game.  Players can create and destroy
various types of blocks in a three-dimensional open world.  This allows
forming structures in every possible creation, on multiplayer servers or as a
single player.  Mods and texture packs allow players to personalize the game
in different ways.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

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
   (home-page "http://www.eblong.com/zarf/glk/")
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
   (home-page "http://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using the Glk API.")
   (license license:expat)))

(define-public fizmo
  (package
    (name "fizmo")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://christoph-ender.de/fizmo/source/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w7cgyjrhgkadjrazijzhq7zh0pl5bfc6wl7mdpgh020y4kp46d7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (let ((libjpeg (assoc-ref %build-inputs "libjpeg"))
             (ncurses (assoc-ref %build-inputs "ncurses")))
         (list (string-append "jpeg_CFLAGS=-I" libjpeg "/include")
               (string-append "jpeg_LIBS=-ljpeg")
               (string-append "ncursesw_CFLAGS=-I" ncurses "/include")
               (string-append "ncursesw_LIBS=-lncursesw")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("sdl" ,sdl)))
    (home-page "https://christoph-ender.de/fizmo/")
    (synopsis "Z-machine interpreter")
    (description
     "Fizmo is a console-based Z-machine interpreter.  It is used to play
interactive fiction, also known as text adventures, which were implemented
either by Infocom or created using the Inform compiler.")
    (license license:bsd-3)))

(define-public retroarch
  (package
    (name "retroarch")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libretro/RetroArch/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rbdax3i33myg1v938pxy28117ihff2lml1ky6g70c8099fkirjx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc")))
               (substitute* "qb/qb.libs.sh"
                 (("/bin/true") (which "true")))
               ;; The configure script does not yet accept the extra arguments
               ;; (like ‘CONFIG_SHELL=’) passed by the default configure phase.
               (zero? (system*
                       "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--global-config-dir=" etc)))))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "http://www.libretro.com/")
    (synopsis "Reference frontend for the libretro API")
    (description
     "Libretro is a simple but powerful development interface that allows for
the easy creation of emulators, games and multimedia applications that can plug
straight into any libretro-compatible frontend.  RetroArch is the official
reference frontend for the libretro API, currently used by most as a modular
multi-system game/emulator system.")
    (license license:gpl3+)))

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
    (inputs `(("readline" ,readline)))
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
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "0d2j4ybdjmimg67v2fndgahgq4fvgz3fpfb3a4l1ar75n6hy776s"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("sfml" ,sfml)))
    (synopsis "High speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "https://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))

(define-public supertuxkart
  (package
    (name "supertuxkart")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/supertuxkart/SuperTuxKart/"
                           version "/supertuxkart-" version "-src.tar.xz"))
       (sha256
        (base32
         "10l2ljmd7mv8f9ylarqmxxryicdnph2qkm3g5maxnsm2k2q0n20b"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled library sources
        '(begin
           ;; FIXME: try to unbundle enet, and angelscript
           (for-each delete-file-recursively
                     '("lib/zlib"
                       "lib/libpng"
                       "lib/jpeglib"
                       "lib/glew"
                       "lib/wiiuse"))
           (substitute* "CMakeLists.txt"
             ;; Supertuxkart uses modified versions of the Irrlicht engine
             ;; and the bullet library.  The developers gave an explanation here:
             ;; http://forum.freegamedev.net/viewtopic.php?f=17&t=3906
             (("add_subdirectory\\(.*/(glew|zlib)\"\\)") ""))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags
       (list "-DUSE_WIIUSE=0"
             ;; Do not use the bundled zlib
             "-DNO_IRR_COMPILE_WITH_ZLIB_=TRUE"
             ;; Irrlicht returns an integer instead of a boolean
             "-DCMAKE_C_FLAGS=-fpermissive")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("glew")
                (string-append (assoc-ref inputs "glew")
                               "/lib/libGLEW.a"))
               (("include_directories\\(\"\\$\\{PROJECT_SOURCE_DIR\\}/lib/glew/include\"\\)")
                (string-append "include_directories(\""
                               (assoc-ref inputs "glew")
                               "/include\")")))
             #t)))))
    (inputs
     `(("glew" ,glew)
       ("zlib" ,zlib)
       ("openal" ,openal)
       ("libvorbis" ,libvorbis)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("mesa" ,mesa)
       ("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("curl" ,curl)
       ;; The following input is needed to build the bundled and modified
       ;; version of irrlicht.
       ("libjpeg" ,libjpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://supertuxkart.net")
    (synopsis "3D kart racing game")
    (description "SuperTuxKart is a 3D kart racing game, with a focus on
having fun over realism.  You can play with up to 4 friends on one PC, racing
against each other or just trying to beat the computer; single-player mode is
also available.")
    (license license:gpl3+)))

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
          (lambda _ (setenv "LIBS" "-lm"))))))
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)))
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
    (version "1.12.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wesnoth/wesnoth-"
                                  (version-major+minor version) "/wesnoth-"
                                  version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0kifp6g1dsr16m6ngjq2hx19h851fqg326ps3krnhpyix963h3x5"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:configure-flags
       ;; XXX: Failed to compile with '-Werror=old-style-cast'.
       ;;   boost/mpl/assert.hpp:313:58: error:
       ;;     use of old-style cast [-Werror=old-style-cast]
       ;;   [...]
       ;;   cc1plus: all warnings being treated as errors
       '("-DENABLE_STRICT_COMPILATION=OFF")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("dbus" ,dbus)
       ("fribidi" ,fribidi)
       ("libvorbis" ,libvorbis)
       ("pango" ,pango)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-net" ,sdl-net)
       ("sdl-ttf" ,sdl-ttf)))
    (home-page "http://www.wesnoth.org/")
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

(define-public dosbox
  (package
    (name "dosbox")
    (version "0.74.svn3947")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "http://svn.code.sf.net/p/dosbox/code-0/dosbox/trunk/")
                    (revision 3947)))
              (file-name (string-append name "-" version "-checkout"))
              ;; Use SVN head, since the last release (2010) is incompatible
              ;; with GCC 4.8+ (see
              ;; <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=624976>).
              (sha256
               (base32
                "1p918j6090d1nkvgq7ifvmn506zrdmyi32y7p3ms40d5ssqjg8fj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'autogen.sh
                   (lambda _
                     (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("sdl" ,sdl)
       ("libpng" ,libpng)
       ("zlib" ,zlib)
       ("alsa-lib" ,alsa-lib)
       ("glu" ,glu)
       ("mesa" ,mesa)))
    (home-page "http://www.dosbox.com")
    (synopsis "X86 emulator with CGA/EGA/VGA/etc. graphics and sound")
    (description "DOSBox is a DOS-emulator that uses the SDL library.  DOSBox
also emulates CPU:286/386 realmode/protected mode, Directory
FileSystem/XMS/EMS, Tandy/Hercules/CGA/EGA/VGA/VESA graphics, a
SoundBlaster/Gravis Ultra Sound card for excellent sound compatibility with
older games.")
    (license license:gpl2+)))

(define-public gamine
  (package
    (name "gamine")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gamine-game/"
                                  "gamine-" version ".tar.gz"))
              (sha256
               (base32
                "1iny959i1kl2ab6z5xi4s66mrvrwcarxyvjfp2k1sx532s8knk8h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base) ;playbin plugin
       ("gst-plugins-good" ,gst-plugins-good) ;for wav playback
       ("gtk+" ,gtk+)))
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
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
            #t)))))
    (home-page "http://gamine-game.sourceforge.net/")
    (synopsis "Mouse and keyboard discovery for children")
    (description
     "Gamine is a game designed for young children who are learning to use the
mouse and keyboard.  The child uses the mouse to draw colored dots and lines
on the screen and keyboard to display letters.")
    ;; Most files under gpl2+ or gpl3+, but eat.wav under gpl3
    (license license:gpl3)))

(define-public raincat
  (package
    (name "raincat")
    (version "1.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/Raincat/Raincat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1aalh68h6799mv4vyg30zpskl5jkn6x2j1jza7p4lrflyifxzar8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-random" ,ghc-random)
       ("ghc-glut" ,ghc-glut)
       ("freeglut" ,freeglut)
       ("ghc-opengl" ,ghc-opengl)
       ("ghc-sdl" ,ghc-sdl)
       ("ghc-sdl-image" ,ghc-sdl-image)
       ("ghc-sdl-mixer" ,ghc-sdl-mixer)))
    (home-page "http://www.bysusanlin.com/raincat/")
    (synopsis "Puzzle game with a cat in lead role")
    (description "Project Raincat is a game developed by Carnegie Mellon
students through GCS during the Fall 2008 semester.  Raincat features game
play inspired from classics Lemmings and The Incredible Machine.  The project
proved to be an excellent learning experience for the programmers.  Everything
is programmed in Haskell.")
    (license license:bsd-3)))

(define-public manaplus
  (package
    (name "manaplus")
    (version "1.7.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://repo.manaplus.org/manaplus/download/"
                    version "/manaplus-" version ".tar.xz"))
              (sha256
               (base32
                "0mbxzsgjg16pqa3jnxkd7wwvw1lrx455r7fvwjfhzp0yv7acrn10"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("curl" ,curl)
       ("libxml2" ,libxml2)
       ("mesa" ,mesa)
       ("physfs" ,physfs)
       ("sdl-union" ,(sdl-union))))
    (home-page "http://manaplus.org")
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

(define-public mupen64plus-core
  (package
    (name "mupen64plus-core")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-core/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dg2hksm5qni2hcha93k7n4fqr92888p946f7phb0ndschzfh9kk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("freetype" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "all" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    ;; As per the Makefile (in projects/unix/Makefile):
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Nintendo 64 emulator core library")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
core library.")
    (license license:gpl2+)))

(define-public mupen64plus-audio-sdl
  (package
    (name "mupen64plus-audio-sdl")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-audio-sdl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ss6w92n2rpfnazhg9lbq0nvs3fqx93nliz3k3wjxdlx4dpi7h3a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL audio plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-input-sdl
  (package
    (name "mupen64plus-input-sdl")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-input-sdl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11sj5dbalp2nrlmki34vy7wy28vc175pnnkdk65p8599hnyq37ri"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
SDL input plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-hle
  (package
    (name "mupen64plus-rsp-hle")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-rsp-hle/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15h7mgz6xd2zjzm6l3f96sbs8kwr3xvbwzgikhnka79m6c69hsxv"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
high-level emulation (HLE) RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-rsp-z64
  (package
    (name "mupen64plus-rsp-z64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-rsp-z64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10jz1w2dhx5slhyk4m8mdqlpsd6cshchslr1fckb2ayzb1ls3ghi"))))
    (build-system gnu-build-system)
    (inputs
     `(("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 RSP processor plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-arachnoid
  (package
    (name "mupen64plus-video-arachnoid")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-arachnoid/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jjwf144rihznm4lnqbhgigxw664v3v32wy94adaa6imk8z6gslh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Arachnoid video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64
  (package
    (name "mupen64plus-video-glide64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-glide64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rm55dbf6xgsq1blbzs6swa2ajv0qkn38acbljj346abnk6s3dla"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-glide64mk2
  (package
    (name "mupen64plus-video-glide64mk2")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-glide64mk2/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ihl4q293d6svba26b4mhapjcdg12p90gibz79b4mx423jlcxxj9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("boost" ,boost)
       ("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Glide64MK2 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-rice
  (package
    (name "mupen64plus-video-rice")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-rice/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rd2scjmh285w61aj3mgx71whg5rqrjbry3cdgicczrnyvf8wdvk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("libpng" ,libpng)
       ("mesa" ,mesa)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Rice Video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Rice Video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-video-z64
  (package
    (name "mupen64plus-video-z64")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-video-z64/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x7wsjs5gx2iwx20p4cjcbf696zsjlh31qxmghwv0ifrq8x58s1b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("glew" ,glew)
       ("mupen64plus-core" ,mupen64plus-core)
       ("sdl2" ,sdl2)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix")))
         ;; XXX Should be unnecessary with the next release.
         (add-before
          'build 'use-sdl2
          (lambda _
            (substitute* "Makefile"
              (("SDL_CONFIG = (.*)sdl-config" all prefix)
               (string-append "SDL_CONFIG = " prefix "sdl2-config"))))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus Z64 video plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
Z64 video plugin.")
    (license license:gpl2+)))

(define-public mupen64plus-ui-console
  (package
    (name "mupen64plus-ui-console")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mupen64plus/mupen64plus-ui-console/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qkpd8ic7xsgnqz7spl00wxdygf79m7d1k8rabbygjk5lg6p8z2"))
       (patches (search-patches "mupen64plus-ui-console-notice.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("sdl2" ,sdl2)))
    ;; Mupen64Plus supports a single data directory and a single plugin
    ;; directory in its configuration, yet we need data and plugin files from
    ;; a variety of packages.  The best way to deal with this is to install
    ;; all packages from which data and plugin files are needed into one's
    ;; profile, and point the configuration there.  Hence, propagate the most
    ;; important packages here to save the user from the bother.  The patch
    ;; mupen64plus-ui-console-notice also gives users instructions on what
    ;; they need to do in order to point the configuration to their profile.
    (propagated-inputs
     `(("mupen64plus-core" ,mupen64plus-core)
       ("mupen64plus-audio-sdl" ,mupen64plus-audio-sdl)
       ("mupen64plus-input-sdl" ,mupen64plus-input-sdl)
       ("mupen64plus-rsp-hle" ,mupen64plus-rsp-hle)
       ("mupen64plus-video-glide64" ,mupen64plus-video-glide64)
       ("mupen64plus-video-glide64mk2" ,mupen64plus-video-glide64mk2)
       ("mupen64plus-video-rice" ,mupen64plus-video-rice)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The mupen64plus build system has no configure phase.
         (delete 'configure)
         ;; Makefile is in a subdirectory.
         (add-before
          'build 'cd-to-project-dir
          (lambda _
            (chdir "projects/unix"))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out"))
             (m64p (assoc-ref %build-inputs "mupen64plus-core")))
         (list "all"
               (string-append "PREFIX=" out)
               (string-append "APIDIR=" m64p "/include/mupen64plus")
               ;; Trailing slash matters here.
               (string-append "COREDIR=" m64p "/lib/")))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://www.mupen64plus.org/")
    (synopsis "Mupen64Plus SDL input plugin")
    (description
     "Mupen64Plus is a cross-platform plugin-based Nintendo 64 (N64) emulator
which is capable of accurately playing many games.  This package contains the
command line user interface.  Installing this package is the easiest way
towards a working Mupen64Plus for casual users.")
    (license license:gpl2+)))

(define-public nestopia-ue
  (package
    (name "nestopia-ue")
    (version "1.47")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rdanbrook/nestopia/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dzrrjmvyqks64q5l5pfly80jb6qcsbj5b3dm40fijd5xnpbapci"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; We don't need libretro for the GNU/Linux build.
                  (delete-file-recursively "libretro")
                  ;; Use system zlib.
                  (delete-file-recursively "source/zlib")
                  (substitute* "source/core/NstZlib.cpp"
                    (("#include \"../zlib/zlib.h\"") "#include <zlib.h>"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("ao" ,ao)
       ("glu" ,glu)
       ("gtk+" ,gtk+)
       ("libarchive" ,libarchive)
       ("mesa" ,mesa)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The Nestopia build system consists solely of a Makefile.
         (delete 'configure)
         (add-before 'build 'remove-xdg-desktop-menu-call
           (lambda _
             (substitute* "Makefile"
               (("xdg-desktop-menu install .*") ""))))
         (add-before 'build 'remove-gdkwayland-include
           (lambda _
             (substitute* "source/unix/gtkui/gtkui.h"
               (("#include <gdk/gdkwayland\\.h>") "")))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "CC=gcc" "CXX=g++" (string-append "PREFIX=" out)))
       ;; There are no tests.
       #:tests? #f))
    (home-page "http://0ldsk00l.ca/nestopia/")
    (synopsis "Nintendo Entertainment System (NES/Famicom) emulator")
    (description
     "Nestopia UE (Undead Edition) is a fork of the Nintendo Entertainment
System (NES/Famicom) emulator Nestopia, with enhancements from members of the
emulation community.  It provides highly accurate emulation.")
    (license license:gpl2+)))

(define-public emulation-station
  (let ((commit "646bede3d9ec0acf0ae378415edac136774a66c5"))
    (package
      (name "emulation-station")
      (version "2.0.1")
      (source (origin
                (method git-fetch) ; no tarball available
                (uri (git-reference
                      (url "https://github.com/Aloshi/EmulationStation.git")
                      (commit commit))) ; no version tag
                (sha256
                 (base32
                  "0cm0sq2wri2l9cvab1l0g02za59q7klj0h3p028vr96n6njj4w9v"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f)) ; no tests
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("boost" ,boost)
         ("curl" ,curl)
         ("eigin" ,eigen)
         ("freeimage" ,freeimage)
         ("freetype" ,freetype)
         ("mesa" ,mesa)
         ("sdl2" ,sdl2)))
      (synopsis "Video game console emulator front-end")
      (description "EmulationStation provides a graphical front-end to a large
number of video game console emulators.  It features an interface that is
usable with any game controller that has at least 4 buttons, theming support,
and a game metadata scraper.")
      (home-page "http://www.emulationstation.org")
      (license license:expat))))

(define openttd-engine
  (package
    (name "openttd-engine")
    (version "1.6.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "http://binaries.openttd.org/releases/"
                                 version "/openttd-" version "-source.tar.xz"))
             (sha256
              (base32
               "1ak32fj5xkk2fvmm3g8i7wzmk4bh2ijsp8fzvvw5wj6365p9j24v"))
             (modules '((guix build utils)))
             (snippet
              ;; The DOS port contains proprietary software.
              '(delete-file-recursively "os/dos"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f              ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; The build process fails if the configure script is passed the
         ;; option "--enable-fast-install".
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lzo (assoc-ref inputs "lzo")))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         ;; Provide the "lzo" path.
                         (string-append "--with-liblzo2="
                                        lzo "/lib/liblzo2.a")
                         ;; Put the binary in 'bin' instead of 'games'.
                         "--binary-dir=bin"))))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("allegro" ,allegro-4)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("icu4c" ,icu4c)
       ("libpng" ,libpng)
       ("lzo" ,lzo)
       ("sdl" ,sdl)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (synopsis "Transportation economics simulator")
    (description "OpenTTD is a game in which you transport goods and
passengers by land, water and air.  It is a re-implementation of Transport
Tycoon Deluxe with many enhancements including multiplayer mode,
internationalization support, conditional orders and the ability to clone,
autoreplace and autoupdate vehicles.  This package only includes the game engine.  When you start
it you will be prompted to download a graphics set.")
    (home-page "http://openttd.org/")
    ;; This package is GPLv2, except for a few files located in
    ;; "src/3rdparty/" which are under the 3-clause BSD, LGPLv2.1+ and Zlib
    ;; licenses.  In addition, this software contains an in-game downloader
    ;; from which the user may find non-functional data licensed under
    ;; different terms.
    (license (list license:bsd-3 license:gpl2 license:lgpl2.1+ license:zlib))))

;; TODO Add 'openttd-opengfx' and 'openttd-openmsx' packages and make
;; 'openttd' a wrapper around them.  The engine is playable by itself,
;; but it asks a user to download graphics if it's not found.

(define-public openttd
  (package
    (inherit openttd-engine)
    (name "openttd")))

(define-public pinball
  (package
    (name "pinball")
    (version "0.3.1")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://sourceforge/pinball/pinball/"
                                 "pinball-" version "/"
                                 "pinball-" version ".tar.gz"))
             (sha256
              (base32
               "1f2whlrfidwfh8lvr8cspcyirc6840r5d1ajm7x99qmngygrhixs"))
             (patches (search-patches "pinball-const-fix.patch"
                                      "pinball-cstddef.patch"
                                      "pinball-missing-separators.patch"
                                      "pinball-src-deps.patch"
                                      "pinball-system-ltdl.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)))
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-image")
                            "/include/SDL -I"
                            (assoc-ref %build-inputs "sdl-mixer")
                            "/include/SDL"))))
    (home-page "http://pinball.sourceforge.net")
    (synopsis "Pinball simulator")
    (description "The Emilia Pinball Project is a pinball simulator.  There
are only two levels to play with, but they are very addictive.")
    (license license:gpl2)))

(define-public pioneers
  (package
    (name "pioneers")
    (version "15.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/pio/"
                                  "pioneers-" version ".tar.gz"))
              (sha256
               (base32
                "1p1d18hrfmqcnghip3shkzcs5qkz6j99jvkdkqfi7pqdvjc323cs"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+)
              ("librsvg" ,librsvg)
              ("avahi" ,avahi)))
    (native-inputs `(("intltool" ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Board game inspired by The Settlers of Catan")
    (description "Pioneers is an emulation of the board game The Settlers of
Catan.  It can be played on a local network, on the internet, and with AI
players.")
    (home-page "http://pio.sourceforge.net/")
    (license license:gpl2+)))

(define-public desmume
  (package
    (name "desmume")
    (version "0.9.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/desmume/desmume/"
             version "/desmume-" version ".tar.gz"))
       (sha256
        (base32
         "15l8wdw3q61fniy3h93d84dnm6s4pyadvh95a0j6d580rjk4pcrs"))))
    (build-system gnu-build-system)
    (arguments
     ;; Enable support for WiFi and microphone.
     `(#:configure-flags '("--enable-wifi"
                           "--enable-openal")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("zlib" ,zlib)
       ("sdl" ,sdl)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("glu" ,glu)))
    (home-page "http://desmume.org/")
    (synopsis "Nintendo DS emulator")
    (description
     "DeSmuME is an emulator for the Nintendo DS handheld gaming console.")
    (license license:gpl2)))

(define-public einstein
  (package
    (name "einstein")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://http.debian.net/debian/pool/main/e/"
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
     `(("font-dejavu" ,font-dejavu)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (dejavu (string-append (assoc-ref inputs "font-dejavu")
                                         "/share/fonts/truetype/DejaVuSans.ttf")))
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
    (version "1.2.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.hoopajoo.net/static/projects/powwow-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xmsg2y7qcvj67i9ilnih0mvfxcpni7fzrz343x9rdfnkkzf3pp8"))))
    (inputs
     `(("ncurses" ,ncurses)))
    (build-system gnu-build-system)
    (home-page "http://www.hoopajoo.net/projects/powwow.html")
    (synopsis "MUD and telnet client")
    (description
     "POWWOW is a client software which can be used for telnet as well as for
@dfn{Multi-User Dungeon} (MUD).  Additionally it can serve as a nice client for
the chat server psyced with the specific config located at
http://lavachat.symlynx.com/unix/")
    (license license:gpl2+)))

(define-public red-eclipse
  (let ((data-sources
         '(("acerspyro"   "0zmg78scrfdv33h7vszqvzylcqjwg7d5b0j2riav3rjfh326j8xx")
           ("actors"      "0l00rsvppqzdpsikm5qpj38jiygirszxlzay2nxp4g4n2qjq0m4a")
           ("appleflap"   "0jhfr7f13hk3nswwxqc4jajriipr6zz6j63v955nv4sgxs7lzbjd")
           ("blendbrush"  "1nk0zaisbqf2khrivq8ls6z2lnh6d51m133m2ppxk7k4c9gq1imq")
           ("caustics"    "1hq08k476wayi0kmk4ps8h6jr75yinq04f1r2p8r79xsdpxq9my5")
           ("crosshairs"  "1gmrmjm7i7n9py0qrzamk7ygi63yx1mr2pp6iwz2vwngprl03n8m")
           ("dziq"        "0gr36ydrv8syjxv7w9dw3ix8waaq201fzxr0klkqp260p8xp215s")
           ("elyvisions"  "05syxlpsap6nfwxnnd0ls7qj1p4vhw2jxi41pi5inwpfifapfphz")
           ("fonts"       "184syks602xc657q08973w5ji50x5zssvd4vp2q2ig8m68iyr51c")
           ("freezurbern" "020gpgcpy4rqjd9d18npfm96j8f02jcjnccbxcgzk1yb58y687ya")
           ("john"        "0hj5kwlb2gb0gsnl9bk7dkqlk8r7vxcw8gxpgrb3kfn8d9cwcb7k")
           ("jojo"        "0fij06040r7s5p7jksxm7wxi9jqwkhhm8iywys0dagk8j2wcbvsz")
           ("jwin"        "0ysfynjvypc8dszf7rsvk02jgw8fmsli49vy2xpm83zpkrqpddgf")
           ("luckystrike" "1bm0xdqjv35ry5xwbzw3a3v1xf2gj1jwfg29nyl6w3ch0h6crr11")
           ("maps"        "0c9d1zxmpnngwhchzw6xb6cf84cx8xyycmdqcvyhamrd95d96qma")
           ("mayhem"      "133pdql7ari159skd9qdmw0p1m73x32d1v6jswkz0xwk8vgxmkil")
           ("mikeplus64"  "1d5npn9wlw0mviz9vhzzcsj98jvfh1wbvlh1nyqfj4ws5nfxhs7x")
           ("misc"        "19x2ps6yxnfrz0xdhqdwncaq25ds7i4w2l8sdfi95yh2r7c5k1qn")
           ("nieb"        "15029nipl92cb0jbh46z00k51hf3jk4v05pwx266b6b11bapdz0c")
           ("nobiax"      "0k9apim5z4ihd5ajmnbq4gyh24w872dv0mr5v8wqn31a8gxzahhp")
           ("particles"   "06827r9pnhzjil381xiwcbc93v9nxin7qlr59yrvk9gdzxmklk9m")
           ("philipk"     "1l6fhl6qz471vjn05hvk29bm8dhwnzqbmi2hdylpa9k998nzkfc1")
           ("projectiles" "03ay8ik52n3vx723swqlnl5gpkzf1v1gadwj3zcnh43ch7nd2bqh")
           ("props"       "1yxz7gfmb79sqqrkyfdzp4ar9rf5f1kpfij4nrkk1l8vbw9liksc")
           ("skyboxes"    "1mm98mhb6yhb006p1hlic91jcwjxhq79mblxciwbqqa9c5g4yki6")
           ("snipergoth"  "1vlpmwlg71g6l5b706gp82bc07i5bbw2zphzynm2fx49za0zdi44")
           ("sounds"      "156g5wh8cvdh6zr33haqm566sd28ylnzdf2h4pqzpxbb2i19vbfg")
           ("textures"    "0wkhl5cgymr9kslzhksi83hs15rb0q01xvax5khi6b4dcl3mrmsh")
           ("torley"      "1xlag6ndjyqafl984n6d9zi96dv9aif7vrc2nvikc3iwgjwlbxav")
           ("trak"        "12x9ix8zkqn9svy56qmdgj4x2814qh25f4srplgq691lqn9qjhvd")
           ("ulukai"      "0gz1hd8hca2biskc85hw4jjacpsmqg9x4w6cwrka8x987xmc92k5")
           ("unnamed"     "09v8fjy6jqypm1i121kilg3z6zpw7dm0i4gxhd9b7ihprvzvy8r7")
           ("vanities"    "0m3vfq9l71pbb80qz4s3k8r5azmm158chqbw8snch09ymxm6h462")
           ("vegetation"  "07yzm9lbzr624j4i652ny5p762p83gadg40c1k8gwff4y7yk55gn")
           ("weapons"     "05fsp17gdrhjqdwia7rwdw9gcijaqwcnny8lf6krms43xmn8cj0x")
           ("wicked"      "0jjgwzdibr5my369gwvmvbklpjlwq939zgf643rv0168xc087xb2"))))
    (package
      (name "red-eclipse")
      (version "1.5.8")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/red-eclipse/base"
                                    "/archive/v" version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1ah92axwcai0fhgm7pvfb2dxvfdiwwyh8iqyiffndh6782hxz3bc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f            ; no check target
         #:make-flags (list "CC=gcc" "-Csrc"
                            (string-append "INSTDIR="
                                           (assoc-ref %outputs "out") "/bin"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-data
             (lambda* (#:key inputs #:allow-other-keys)
               (delete-file-recursively "data")
               (mkdir "data")
               (for-each (lambda (name)
                           (system* "tar" "-xvf"
                                    (assoc-ref inputs name)
                                    "-Cdata"
                                    "--transform"
                                    (string-append "s/"
                                                   name "-" ,version "/"
                                                   name "/")))
                         (list ,@(map car data-sources)))
               #t))
           (delete 'configure)  ; no configure script
           (add-after 'set-paths 'set-sdl-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "sdl-union")
                                      "/include/SDL2"))
               #t))
           (add-after 'install 'copy-data
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "config"
                                   (string-append out "/config"))
                 (copy-file "doc/examples/servinit.cfg"
                            (string-append out "/config/servinit.cfg"))
                 (copy-recursively "data"
                                   (string-append out "/data")))
               #t))
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
                   (chmod "redeclipse_server_linux" #o555)))
               #t)))))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (inputs
       `(("curl" ,curl)
         ("glu" ,glu)
         ("sdl-union" ,(sdl-union (list sdl2
                                        sdl2-image
                                        sdl2-mixer)))
         ;; Create origin records for the many separate data packages.
         ,@(map (match-lambda
                  ((name hash)
                   (list name
                         (origin
                           (method url-fetch)
                           (uri (string-append
                                 "https://github.com/red-eclipse/"
                                 name "/archive/v" version ".tar.gz"))
                           (sha256 (base32 hash))
                           (file-name (string-append name "-" version
                                                     ".tar.gz"))))))
                data-sources)))
      (home-page "http://redeclipse.net/")
      (synopsis "Arena shooter derived from the Cube 2 engine")
      (description
       "Red Eclipse is an arena shooter, created from the Cube2 engine.
Offering an innovative parkour system and distinct but all potent weapons,
Red Eclipse provides fast paced and accessible gameplay.")
      ;; The engine is under Zlib; data files are covered by the other
      ;; licenses.  More details at <http://redeclipse.net/wiki/License>.
      (license (list license:expat
                     license:zlib
                     license:cc-by-sa3.0
                     license:cc-by3.0
                     license:cc0)))))

(define-public higan
  (package
    (name "higan")
    (version "101")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/higan/higan/repository/archive.tar.gz?ref=v"
             version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qavwkmzc63p6qplmxii4gc541z5mcs8gjwh3m4y7i576r7rcbk9"))
       (patches (search-patches "higan-remove-march-native-flag.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("eudev" ,eudev)
       ("gtk+" ,gtk+-2)
       ("gtksourceview-2" ,gtksourceview-2)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl)))
    (arguments
     '(#:phases
       (let ((build-phase (assoc-ref %standard-phases 'build))
             (install-phase (assoc-ref %standard-phases 'install)))
         (modify-phases %standard-phases
           ;; The higan build system has no configure phase.
           (delete 'configure)
           (add-before 'build 'chdir-to-higan
             (lambda _
               (chdir "higan")))
           (add-before 'install 'create-/share/applications
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; It seems the author forgot to do this in the Makefile.
                 (mkdir-p (string-append out "/share/applications")))))
           (add-after 'install 'chdir-to-icarus
             (lambda _
               (chdir "../icarus")))
           (add-after 'chdir-to-icarus 'build-icarus build-phase)
           (add-after 'build-icarus 'install-icarus install-phase)
           (add-after 'install-icarus 'wrap-higan-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (higan (string-append bin "/higan"))
                      (higan-original (string-append higan "-original"))
                      (bash (string-append (assoc-ref inputs "bash")
                                           "/bin/bash"))
                      (coreutils (assoc-ref inputs "coreutils"))
                      (mkdir (string-append coreutils "/bin/mkdir"))
                      (cp (string-append coreutils "/bin/cp"))
                      (cp-r (string-append cp " -r --no-preserve=mode")))
                 ;; First, have the executable make sure ~/.local/share/higan
                 ;; contains up to date files.  Higan insists on looking there
                 ;; for these data files.
                 (rename-file higan higan-original)
                 (with-output-to-file higan
                   (lambda ()
                     (display
                      (string-append
                       "#!" bash "\n"
                       ;; higan doesn't respect $XDG_DATA_HOME
                       mkdir " -p ~/.local/share\n"
                       cp-r " " out "/share/higan ~/.local/share\n"
                       "exec " higan-original))))
                 (chmod higan #o555)
                 ;; Second, make sure higan will find icarus in PATH.
                 (wrap-program higan
                   `("PATH" ":" prefix (,bin))))))))
       #:make-flags
       (list "compiler=g++"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       ;; There is no test suite.
       #:tests? #f))
    (home-page "http://byuu.org/emulation/higan/")
    (synopsis "Nintendo multi-system emulator")
    (description
     "higan (formerly bsnes) is an emulator for multiple Nintendo video game
consoles, including the Nintendo Entertainment System (NES/Famicom), Super
Nintendo Entertainment System (SNES/Super Famicom), Game Boy, Game Boy
Color (GBC), and Game Boy Advance (GBA).  It also supports the subsystems
Super Game Boy, BS-X Satellaview, and Sufami Turbo.")
    ;; As noted in these files among more:
    ;; - icarus/icarus.cpp
    ;; - higan/emulator/emulator.hpp
    (license license:gpl3)))

(define-public grue-hunter
  (package
    (name "grue-hunter")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://jxself.org/" name ".tar.gz"))
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
                     (begin
                       (copy-file tarball "grue-hunter.tar.gz")
                       (zero? (system* gzip "-d" "grue-hunter.tar.gz"))
                       (zero? (system* tar "xvf"  "grue-hunter.tar"))

                       (mkdir-p bin)
                       (copy-file "grue-hunter/gh.pl"
                                  (string-append bin "/grue-hunter"))
                       (patch-shebang (string-append bin "/grue-hunter")
                                      (list perl))

                       (install-file "grue-hunter/AGPLv3.txt" doc))))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (home-page "http://jxself.org/grue-hunter.shtml")
    (synopsis "Text adventure game")
    (description
     "Grue Hunter is a text adventure game written in Perl.  You must make
your way through an underground cave system in search of the Grue.  Can you
capture it and get out alive?")
    (license license:agpl3+)))

(define-public warzone2100
  (package
    (name "warzone2100")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name
                                  "/releases/" version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1nd609s0g4sya3r4amhkz3f4dpdmm94vsd2ii76ap665a1nbfrhg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-tests-with-qt
           (lambda _
             (substitute* "tests/Makefile.in"
               (("(framework_linktest_LDADD|maptest_LDADD) = " prefix)
                (string-append prefix "$(QT5_LIBS) ")))
             #t))
         (add-after 'unpack 'remove-reference-to-missing-file
           (lambda _
             (substitute* "icons/Makefile.in"
               (("\\$\\(INSTALL_DATA\\) \\$\\(srcdir\\)/warzone2100.appdata.xml.*") ""))
             #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("unzip" ,unzip)
                     ("zip" ,zip)))
    (inputs `(("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("fribidi" ,fribidi)
              ("glew" ,glew)
              ("libtheora" ,libtheora)
              ("libvorbis" ,libvorbis)
              ("libxrandr" ,libxrandr)
              ("openal" ,openal)
              ("physfs" ,physfs)
              ("qt" ,qt)
              ("openssl" ,openssl)
              ("quesoglc" ,quesoglc)
              ("sdl2" ,sdl2)))
    (home-page "http://wz2100.net")
    (synopsis "3D Real-time strategy and real-time tactics game")
    (description
     "Warzone 2100 offers campaign, multi-player, and single-player skirmish
modes. An extensive tech tree with over 400 different technologies, combined
with the unit design system, allows for a wide variety of possible units and
tactics.")
    ; Everything is GPLv2+ unless otherwise specified in COPYING.NONGPL
    (license (list license:bsd-3
                   license:cc0
                   license:cc-by-sa3.0
                   license:expat
                   license:gpl2+
                   license:lgpl2.1+))))

(define-public starfighter
  (package
    (name "starfighter")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/starfighter/"
                    (version-major+minor version) "/"
                    name "-" version "-src.tar.gz"))
              (sha256
               (base32
                "1646hpjq8bz2fkfkja1dah511hn7zd2r7da4w9c9blhad3p5732v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (home-page "http://starfighter.nongnu.org/")
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
              (uri (string-append "mirror://sourceforge/" name
                                  "/Chromium B.S.U. source code/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jk2w5b6s6nkzri585bbz16cif2fhqcnl5l1mq3rd98r9nil3hd1"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("gettext" ,gettext-minimal)
              ("glu" ,glu)
              ("quesoglc" ,quesoglc)
              ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))))
    (home-page "http://chromium-bsu.sourceforge.net/")
    (synopsis "Fast-paced, arcade-style, top-scrolling space shooter")
    (description
     "In this game you are the captain of the cargo ship Chromium B.S.U. and
are responsible for delivering supplies to the troops on the front line.  Your
ship has a small fleet of robotic fighters which you control from the relative
safety of the Chromium vessel.")
    ;; Clarified Artistic License for everything but sound, which is covered
    ;; by the Expat License.
    (license (list license:clarified-artistic license:expat))))

(define-public tuxpaint
  (package
    (name "tuxpaint")
    (version "0.9.22")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint/"
                           version "/tuxpaint-" version ".tar.gz"))
       (sha256
        (base32
         "1qrbrdck9yxpcg3si6jb9i11w8lw9h4hqad0pfaxgyiniqpr7gca"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove win32 directory which contains binary dll's and the
           ;; deprecated visualc directory.
           (for-each delete-file-recursively '("win32" "visualc"))
           (substitute* "Makefile"
             ;; Do not rely on $(GPERF) being an absolute file name
             (("\\[ -x \\$\\(GPERF\\) \\]")
              "$(GPERF) --version >/dev/null 2>&1"))))
       (patches (search-patches "tuxpaint-stamps-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("fribidi" ,fribidi)
       ("gettext" ,gettext-minimal)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libpaper" ,libpaper)
       ("netpbm" ,netpbm)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf sdl-image)))))
    ;; TODO: Use system fonts rather than those in data/fonts
    (arguments
     `(#:make-flags `("VER_DATE=2014-08-23"
                      "GPERF=gperf" "CC=gcc"
                      "SDL_PCNAME=sdl SDL_image SDL_mixer SDL_ttf"
                      ,(string-append "PREFIX=" %output)
                      "GNOME_PREFIX=$(PREFIX)"
                      "COMPLETIONDIR=$(PREFIX)/etc/bash_completion.d")
       #:parallel-build? #f             ;fails on some systems
       #:tests? #f                      ;No tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
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
    (version "2014.08.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-stamps/"
                           (string-map (λ (x) (if (eq? x #\.) #\- x)) version)
                           "/tuxpaint-stamps-" version ".tar.gz"))
       (sha256
        (base32
         "0rhlwrjz44wp269v3rid4p8pi0i615pzifm1ym6va64gn1bms06q"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (setenv "PATH"
                           (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
                   (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
                   (chdir (string-append ,name "-" ,version))
                   (let ((dir (string-append %output "/share/tuxpaint/stamps")))
                     (mkdir-p dir)
                     (copy-recursively "stamps" dir)))))
    (home-page (package-home-page tuxpaint))
    (synopsis "Stamp images for Tux Paint")
    (description
     "This package contains a set of \"Rubber Stamp\" images which can be used
with the \"Stamp\" tool within Tux Paint.")
    (license license:gpl2+)))

(define-public tuxpaint-config
  (package
    (name "tuxpaint-config")
    (version "0.0.13")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-config/"
                           version "/tuxpaint-config-" version ".tar.gz"))
       (sha256
        (base32
         "1z12s46mvy87qs3vgq9m0ki9pp21zqc52mmgphahpihw3s7haf6v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("fltk" ,fltk)
       ("libpaper" ,libpaper)
       ;; TODO: Should the following be propagated by fltk?
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("mesa" ,mesa)))
    (arguments
     `(#:make-flags `("VER_DATE=2014-08-23"
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
   (version "0.5.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/SuperTux/supertux/"
                                "releases/download/v" version "/SuperTux-v"
                                version "-Source.tar.gz"))
            (sha256
             (base32
              "1i8avad7w7ikj870z519j383ldy29r6f956bs38cbr8wk513pp69"))))
   (arguments
    '(#:tests? #f
      #:configure-flags '("-DINSTALL_SUBDIR_BIN=bin"
                          "-DENABLE_BOOST_STATIC_LIBS=OFF")))
   (build-system cmake-build-system)
   (inputs `(("sdl2" ,sdl2)
             ("sdl2-image" ,sdl2-image)
             ("sdl2-mixer" ,sdl2-mixer)
             ("openal" ,openal)
             ("mesa" ,mesa)
             ("glew" ,glew)
             ("libvorbis" ,libvorbis)
             ("libogg" ,libogg)
             ("physfs" ,physfs)
             ("curl" ,curl)
             ("boost" ,boost)))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (synopsis "2D platformer game")
   (description "SuperTux is a free classic 2D jump'n run sidescroller game
in a style similar to the original Super Mario games covered under
the GNU GPL.")
   (home-page "https://supertuxproject.org/")
   (license license:gpl3+)))

(define-public tintin++
  (package
    (name "tintin++")
    (version "2.01.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/tintin"
                                  "/TinTin++ Source Code/" version
                                  "/tintin" "-" version ".tar.gz"))
              (sha256
               (base32
                "13h39agyhlhm17zyqlb56bmbbxpimikyf5pana3gd3ylvqy1xq81"))))
    (inputs
     `(("gnutls" ,gnutls)
       ("pcre" ,pcre)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (arguments
     '(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         ;; The source is in tt/src.
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t)))))
    (build-system gnu-build-system)
    (home-page "http://tintin.sourceforge.net/")
    (synopsis "MUD client")
    (description
     "TinTin++ is a MUD client which supports MCCP (Mud Client Compression Protocol),
MMCP (Mud Master Chat Protocol), xterm 256 colors, most TELNET options used by MUDs,
as well as those required to login via telnet on Linux / Mac OS X servers, and an
auto mapper with a VT100 map display.")
    (license license:gpl2+)))

(define-public laby
  (package
    (name "laby")
    (version "0.6.4")
    (source
     (origin (method url-fetch)
             (uri (string-append
                   "https://github.com/sgimenez/laby/archive/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "0gyrfa95l1qka7gbjf7l6mk7mbfvph00l0c995ia272qdw7rjhyf"))
             (patches (search-patches "laby-make-install.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("lablgtk" ,lablgtk)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'setenv
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lablgtk (assoc-ref inputs "lablgtk")))
               (setenv "LD_LIBRARY_PATH"
                       (string-append lablgtk "/lib/ocaml/stublibs"))))))
       #:tests? #f ; no 'check' target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")) "all")))
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
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/porridge/bambam/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10w110mjdwbvddzihh9rganvvjr5jfiz8cs9n7w12zndwwcc3ria"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (add-before 'install 'patch-data-dir-location
           (lambda _
             (substitute* "bambam.py"
               (("'data'") "'../share/bambam/data'"))
             #t))
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
     `(("python-pygame" ,python-pygame)))
    (home-page "https://github.com/porridge/bambam")
    (synopsis "Keyboard mashing and doodling game for babies")
    (description "Bambam is a simple baby keyboard (and gamepad) masher
application that locks the keyboard and mouse and instead displays bright
colors, pictures, and sounds.")
    (license license:gpl3+)))

(define-public mrrescue
  (package
    (name "mrrescue")
    (version "1.02e")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SimonLarsen/mrrescue/releases/"
                    "download/" version "/" name version ".love"))
              (file-name (string-append name "-" version ".love"))
              (sha256
               (base32
                "0jwzbwkgp1l5ia6c7s760gmdirbsncp6nfqp7vqdqsfb63la9gl2"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bindir  (string-append out "/bin"))
                (prog    (string-append bindir "/mrrescue"))
                (source  (assoc-ref %build-inputs "source"))
                (bash    (string-append (assoc-ref %build-inputs "bash")
                                        "/bin/bash"))
                (love    (string-append (assoc-ref %build-inputs "love")
                                        "/bin/love")))
           (mkdir-p bindir)
           (with-output-to-file prog
             (lambda ()
               (format #t "#!~a~%" bash)
               (format #t "exec -a mrrescue \"~a\" \"~a\"~%" love source)))
           (chmod prog #o755)
           #t))))
    (inputs
     `(("bash" ,bash)
       ("love" ,love)))
    (home-page "http://tangramgames.dk/games/mrrescue")
    (synopsis "Arcade-style fire fighting game")
    (description
     "Mr. Rescue is an arcade styled 2d action game centered around evacuating
civilians from burning buildings.  The game features fast paced fire
extinguishing action, intense boss battles, a catchy soundtrack and lots of
throwing people around in pseudo-randomly generated buildings.")
    (license (list license:zlib             ; for source code
                   license:cc-by-sa3.0))))  ; for graphics and music assets

(define-public hyperrogue
  (package
    (name "hyperrogue")
    (version "9.4c")
    ;; When updating this package, be sure to update the "hyperrogue-data"
    ;; origin in native-inputs.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.roguetemple.com/z/hyper/"
                    name (string-join (string-split version #\.) "")
                    "-src.tgz"))
              (sha256
               (base32
                "1ri5fllnhqjm3dlnl1xbb9mlv79iigc940vbvcnk0v5k6p58pavq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL"))))
         ;; Fix font and music paths.
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share-dir (string-append out "/share/hyperrogue"))
                    (dejavu-dir (string-append
                                 (assoc-ref inputs "font-dejavu")
                                 "/share/fonts/truetype"))
                    (dejavu-font "DejaVuSans-Bold.ttf")
                    (music-file "hyperrogue-music.txt"))
               (substitute* "graph.cpp"
                 ((dejavu-font)
                  (string-append dejavu-dir "/" dejavu-font)))
               (substitute* "sound.cpp"
                 (((string-append "\\./" music-file))
                  (string-append share-dir "/" music-file))
                 (("sounds/")
                  (string-append share-dir "/sounds/")))
               (substitute* music-file
                 (("\\*/")
                  (string-append share-dir "/sounds/"))))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share-dir (string-append out "/share/hyperrogue")))
               (mkdir-p bin)
               (copy-file "hyper" (string-append bin "/hyperrogue"))
               (install-file "hyperrogue-music.txt" share-dir))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (assoc-ref inputs "hyperrogue-data"))
                    (out (assoc-ref outputs "out"))
                    (sounds (string-append out "/share/hyperrogue/sounds"))
                    (unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip")))
               (and
                ;; Extract media license information into sounds directory.
                (zero?
                 (system* unzip "-j" data
                          (string-append
                           "hyperrogue"
                           (string-join (string-split ,version #\.) "")
                           "-win/sounds/credits.txt") "-d" sounds))
                ;; Extract sounds and music into sounds directory.
                (zero?
                 (system* "unzip" "-j" data
                          (string-append
                           "hyperrogue"
                           (string-join (string-split ,version #\.) "")
                           "-win/*.ogg") "-d" sounds)))))))))
    (native-inputs
     `(("hyperrogue-data"
        ,(origin
           (method url-fetch)
           (uri
            (string-append
             "http://www.roguetemple.com/z/hyper/" name
             (string-join (string-split version #\.) "")
             "-win.zip"))
           (sha256
            (base32
             "1cyyrsnrixygg3zyz97hpsm6jzwbhydiwk3kl0lm7qjnw2nzkhhh"))))
       ("unzip" ,unzip)))
    (inputs
     `(("font-dejavu" ,font-dejavu)
       ("glew" ,glew)
       ("libpng" ,libpng)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-gfx
                                      sdl-mixer
                                      sdl-ttf)))))
    (home-page "http://www.roguetemple.com/z/hyper/")
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
    (license (list license:bsd-3         ; glew.c, mtrand.*
                   license:cc-by-sa3.0   ; music
                   license:cc-by-sa4.0   ; sounds
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
    (inputs `(("sdl-union" ,(sdl-union (list sdl sdl-image)))))
    (synopsis "Shooter with space station destruction")
    (description
     "Kobo Deluxe is an enhanced version of Akira Higuchi's XKobo graphical game
for Un*x systems with X11.")
    (home-page "http://olofson.net/kobodl/")
    (license license:gpl2+)))

(define-public freeciv
  (package
   (name "freeciv")
   (version "2.5.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "http://download.gna.org/freeciv/"
           "stable/freeciv-" version ".tar.bz2"))
     (sha256
      (base32
       "16wrnsx5rmbz6rjs03bhy0vn20i6n6g73lx7fjpai98ixhzc5bfg"))))
   (build-system gnu-build-system)
   (inputs
    `(("curl" ,curl)
      ("cyrus-sasl" ,cyrus-sasl)
      ("gtk+" ,gtk+)
      ("sdl-mixer" ,sdl-mixer)
      ("zlib" ,zlib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (home-page "http://www.freeciv.org/")
   (synopsis "Turn based empire building strategy game")
   (description "Freeciv is a turn based empire building strategy game
inspired by the history of human civilization.  The game commences in
prehistory and your mission is to lead your tribe from the Stone Age
to the Space Age.")
   (license license:gpl2+)))

(define-public no-more-secrets
  (package
    (name "no-more-secrets")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/bartobri/no-more-secrets/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00mzdif859vm75n1igrffh8c07p1nr4rlm3yb7lrkwwxhrw8xzla"))))
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
     `(("ncurses" ,ncurses)))
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
     `(("curl" ,curl)
       ("fontconfig" ,fontconfig)
       ("ftgl" ,ftgl)
       ("glew" ,glew)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("megaglest-data" ,megaglest-data)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("libircclient" ,libircclient)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("lua" ,lua)
       ("sdl2" ,sdl2)
       ("wxwidgets" ,wxwidgets)))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "-DCUSTOM_DATA_INSTALL_PATH="
                            (assoc-ref %build-inputs "megaglest-data")
                            "/share/megaglest")
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
  (let ((commit "8795cd7adc95957883f2d3465eb9036a774667a7")
        (revision "1"))
    (package
      (name "freegish")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/freegish/freegish.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1p1zf5qqagmcpi1db2bs02cnalpy3qiymp6yzan7k1bhmv859gsx"))
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
         (list "-DCMAKE_INSTALL_FHS=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-DATAPATH
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "CMakeLists.txt"
                 (("^option\\(INSTALL_FHS" line)
                  (string-append "add_definitions(-DDATAPATH=\""
                                 (assoc-ref outputs "out") "/share/freegish\")\n"
                                 line)))
               #t)))))
      (inputs
       `(("sdl-union" ,(sdl-union (list sdl sdl-mixer)))
         ("openal" ,openal)
         ("libvorbis" ,libvorbis)
         ("libogg" ,libogg)
         ("mesa" ,mesa)
         ("libpng" ,libpng)
         ("zlib" ,zlib)))
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
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cxong/cdogs-sdl/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08c3y8ijimx6mp0gm90abz4lsnbflqka519q2v0id0096vsc2pxn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCDOGS_DATA_DIR="
                            (assoc-ref %outputs "out")
                            "/share/cdogs-sdl/"))))
    (inputs
     `(("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (home-page "http://cxong.github.io/cdogs-sdl/")
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
                     (string-append (assoc-ref inputs "sdl-union")
                                    "/include/SDL:"
                                    (assoc-ref inputs "python")
                                    "/include/python2.7:"
                                    (getenv "CPLUS_INCLUDE_PATH")))
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
               (zero? (apply system* "make" make-flags)))))
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
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image)))
       ("python" ,python-2)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page "http://kiki.sourceforge.net/")
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
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/teeworlds/teeworlds/"
                                  "archive/" version "-release.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mqhp6xjl75l49050cid36wxyjn1qr0vjx1c709dfg1lkvmgs6l3"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '("src/engine/external/wavpack/"
                              "src/engine/external/zlib/"))
                  #t))
              (patches
               (search-patches "teeworlds-use-latest-wavpack.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Embed path to assets.
             (substitute* "src/engine/shared/storage.cpp"
               (("#define DATA_DIR.*")
                (string-append "#define DATA_DIR \""
                               (assoc-ref outputs "out")
                               "/share/teeworlds/data"
                               "\"")))

             ;; Bam expects all files to have a recent time stamp.
             (for-each (lambda (file)
                         (utime file 1 1))
                       (find-files "."))

             ;; Do not use bundled libraries.
             (substitute* "bam.lua"
               (("if config.zlib.value == 1 then")
                "if true then")
               (("wavpack = .*")
                "wavpack = {}
settings.link.libs:Add(\"wavpack\")\n"))
             (substitute* "src/engine/client/sound.cpp"
               (("#include <engine/external/wavpack/wavpack.h>")
                "#include <wavpack/wavpack.h>"))
             #t))
         (replace 'build
           (lambda _
             (zero? (system* "bam" "-a" "-v" "release"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (data (string-append out "/share/teeworlds/data")))
               (mkdir-p bin)
               (mkdir-p data)
               (for-each (lambda (file)
                           (install-file file bin))
                         '("teeworlds" "teeworlds_srv"))
               (copy-recursively "data" data)
               #t))))))
    ;; FIXME: teeworlds bundles the sources of "pnglite", a two-file PNG
    ;; library without a build system.
    (inputs
     `(("freetype" ,freetype)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image)))
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bam" ,bam)
       ("python" ,python-2)))
    (home-page "https://www.teeworlds.com")
    (synopsis "2D retro multiplayer shooter game")
    (description "Teeworlds is an online multiplayer game.  Battle with up to
16 players in a variety of game modes, including Team Deathmatch and Capture
The Flag.  You can even design your own maps!")
    (license license:bsd-3)))

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
               (zero? (system* "tar" "-xvf"
                               (assoc-ref inputs "fillets-ng-data")
                               "--strip-components=1"
                               "-C" data))))))))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image
                                      sdl-ttf)))
       ("fribidi" ,fribidi)
       ("libx11" ,libx11)
       ("lua" ,lua-5.1)))
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
    (home-page "http://fillets.sourceforge.net/")
    (synopsis "Puzzle game")
    (description "Fish Fillets NG is strictly a puzzle game.  The goal in
every of the seventy levels is always the same: find a safe way out.  The fish
utter witty remarks about their surroundings, the various inhabitants of their
underwater realm quarrel among themselves or comment on the efforts of your
fish.  The whole game is accompanied by quiet, comforting music.")
    (license license:gpl2+)))
