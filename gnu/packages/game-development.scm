;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Kei Kebreau <kei@openmailbox.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Julian Graham <joolean@gmail.com>
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

(define-module (gnu packages game-development)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xml))

(define-public bullet
  (package
    (name "bullet")
    (version "2.86.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bulletphysics/bullet3/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nghzcl84p8di215p7xj0gy1hyy072hw2xk9cnmav9hv6bjb4n60"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list (string-append
                                 "-DBUILD_SHARED_LIBS=ON "
                                 "-DCMAKE_CXX_FLAGS=-fPIC "
                                 (or (getenv "CXXFLAGS") "")))))
    (inputs
     `(("glu" ,glu)
       ("libx11" ,libx11)
       ("mesa" ,mesa)))
    (home-page "http://bulletphysics.org/")
    (synopsis "3D physics engine library")
    (description
     "Bullet is a physics engine library usable for collision detection.  It
is used in some video games and movies.")
    (license license:zlib)))

(define-public deutex
  (package
   (name "deutex")
   (version "4.4.902")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/Doom-Utils/" name
                                "/archive/v" version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "0rwz1yzgd539x4h25kzhar4q02xyxjwfrcpz4m8ixi312a82p7cn"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        ;; The provided configure script takes a restricted number of arguments.
        (replace 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (zero? (system* "./configure" "--prefix"
                                   (assoc-ref %outputs "out")))))
        ;; "make install" is broken for this package.
        ;; Notably, the binaries overrwrite one another upon installation as
        ;; they are all installed to the "bin" file in the output directory,
        ;; and the manual page fails to install because the directory for the
        ;; manual page is not created.
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref %outputs "out"))
                          (bin (string-append out "/bin"))
                          (share (string-append out "/share")))
                     (install-file "deusf" bin)
                     (install-file "deutex" bin)
                     (install-file "deutex.6" (string-append share "/man/man6")))
                   #t)))))
   (home-page "https://github.com/Doom-Utils/deutex")
   (synopsis "WAD file composer for Doom and related games")
   (description
    "DeuTex is a wad composer for Doom, Heretic, Hexen and Strife. It can be
used to extract the lumps of a wad and save them as individual files.
Conversely, it can also build a wad from separate files.  When extracting a
lump to a file, it does not just copy the raw data, it converts it to an
appropriate format (such as PPM for graphics, Sun audio for samples, etc.).
Conversely, when it reads files for inclusion in pwads, it does the necessary
conversions (for example, from PPM to Doom picture format).  In addition,
DeuTex has functions such as merging wads, etc.")
   (license license:gpl2+)))

(define-public gzochi
  (package
    (name "gzochi")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gzochi/gzochi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "166rawdal45kvanhvi0bkzy1d2pwf1p0lzslb287lcnm9vdw97yy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'remove-Werror
                              (lambda _
                                ;; We can't build with '-Werror', notably
                                ;; because deprecated functions of
                                ;; libmicrohttpd are being used.
                                (substitute* (find-files "." "^Makefile\\.in$")
                                  (("-Werror")
                                   ""))
                                #t)))))
    (native-inputs `(("pkgconfig" ,pkg-config)))
    (inputs `(("bdb" ,bdb)
              ("glib" ,glib)
              ("guile" ,guile-2.0)
              ("libmicrohttpd" ,libmicrohttpd)
              ("ncurses" ,ncurses)
              ("sdl" ,sdl)
              ("zlib" ,zlib)))
    (home-page "http://www.nongnu.org/gzochi/")
    (synopsis "Scalable middleware for multiplayer games")
    (description
     "gzochi is a framework for developing massively multiplayer online games.
A server container provides services to deployed games, which are written in
Guile Scheme, that abstract and simplify some of the most challenging and
error-prone aspects of online game development: Concurrency, data persistence,
and network communications.  A very thin client library can be embedded to
provide connectivity for client applications written in any language.")
    (license license:gpl3+)))

(define-public python-sge-pygame
  (package
    (name "python-sge-pygame")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sge-pygame" version))
       (sha256
        (base32
         "1qhrcja1igqkjjn1w425ni5f41mijdq5dpq0ymkhl29xxrf8hnx8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pygame" ,python-pygame)
       ("python-six" ,python-six)))
    (home-page "http://stellarengine.nongnu.org")
    (synopsis "2D game engine for Python")
    (description
     "The SGE Game Engine (\"SGE\", pronounced like \"Sage\") is a
general-purpose 2D game engine.  It takes care of several details fro you so
you can focus on the game itself.  This makes more rapid game development
possible, and it also makes the SGE easy to learn.")
    (license license:lgpl3+)))

(define-public python2-sge-pygame
  (package-with-python2 python-sge-pygame))

(define-public tiled
  (package
    (name "tiled")
    (version "0.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bjorn/tiled/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kcj2blrlfpghjv0qigip2qcbxfx7vv9i8nr4997hkwhsh6i2pjp"))))
    (build-system gnu-build-system)
    (inputs `(("qt" ,qt)
              ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (system* "qmake"
                        (string-append "PREFIX=" out))))))))
    (home-page "http://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))

(define-public sfml
  (package
    (name "sfml")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              ;; Do not fetch the archives from
              ;; http://mirror0.sfml-dev.org/files/ because files there seem
              ;; to be changed in place.
              (uri (string-append "https://github.com/SFML/SFML/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k2fl5xk3ni2q8bsxl0551inx26ww3w6cp6hssvww0wfjdjcirsm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DSFML_INSTALL_PKGCONFIG_FILES=TRUE")
       #:tests? #f)) ; no tests
    (inputs
     `(("mesa" ,mesa)
       ("glew" ,glew)
       ("flac" ,flac)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("xcb-util-image" ,xcb-util-image)
       ("libxrandr" ,libxrandr)
       ("eudev" ,eudev)
       ("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libsndfile" ,libsndfile)
       ("openal" ,openal)))
    (home-page "https://www.sfml-dev.org")
    (synopsis "Simple and Fast Multimedia Library")
    (description
     "SFML provides a simple interface to the various computer components,
to ease the development of games and multimedia applications.  It is composed
of five modules: system, window, graphics, audio and network.")
    (license license:zlib)))

(define-public sfxr
  (package
    (name "sfxr")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.drpetter.se/files/sfxr-sdl-1.2.1.tar.gz"))
              (sha256
               (base32
                "0dfqgid6wzzyyhc0ha94prxax59wx79hqr25r6if6by9cj4vx4ya"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure) ; no configure script
                  (add-before 'build 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("\\$\\(DESTDIR\\)/usr") out))
                        (substitute* "main.cpp"
                          (("/usr/share")
                           (string-append out "/share")))
                        #t))))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)))
    (inputs
     `(("sdl" ,sdl)
       ("gtk+" ,gtk+)))
    (synopsis "Simple sound effect generator")
    (description "Sfxr is a tool for quickly generating simple sound effects.
Originally created for use in video game prototypes, it can generate random
sounds from presets such as \"explosion\" or \"powerup\".")
    (home-page "http://www.drpetter.se/project_sfxr.html")
    (license license:expat)))

(define-public physfs
  (package
    (name "physfs")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://icculus.org/physfs/downloads/physfs-"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sbbyqzqhyf0g68fcvvv20n3928j0x6ik1njmhn1yigvq2bj11na"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://icculus.org/physfs")
    (synopsis "File system abstraction library")
    (description
     "PhysicsFS is a library to provide abstract access to various archives.
It is intended for use in video games.  For security, no file writing done
through the PhysicsFS API can leave a defined @emph{write directory}.  For
file reading, a @emph{search path} with archives and directories is defined,
and it becomes a single, transparent hierarchical file system.  So archive
files can be accessed in the same way as you access files directly on a disk,
and it makes it easy to ship a new archive that will override a previous
archive on a per-file basis.")
    (license license:zlib)))

(define-public love
  (package
    (name "love")
    (version "0.10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/rude/love/downloads/"
                                 "love-" version "-linux-src.tar.gz"))
             (sha256
              (base32
               "11x346pw0gqad8nmkmywzx4xpcbfc3dslbrdw5x94n1i25mk0sxj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("devil" ,devil)
       ("freetype" ,freetype)
       ("libmodplug" ,libmodplug)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("luajit" ,luajit)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (synopsis "2D game framework for Lua")
    (description "LÖVE is a framework for making 2D games in the Lua
programming language.")
    (home-page "https://love2d.org/")
    (license license:zlib)))

(define-public allegro-4
  (package
    (name "allegro")
    (version "4.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/allegro/allegro/"
                                  version "/allegro-" version ".tar.gz"))
              (sha256
               (base32
                "1p0ghkmpc4kwij1z9rzxfv7adnpy4ayi0ifahlns1bdzgmbyf88v"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             ;; Build addons as shared libraries.  Trying to set ADDON_LINKAGE
             ;; via a command line option doesn't work because it is
             ;; unconditionally clobbered in the build script.
             (substitute* '("CMakeLists.txt")
               (("ADDON_LINKAGE STATIC")
                "ADDON_LINKAGE SHARED"))
             #t)))))
    (inputs
     `(("glu" ,glu)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "http://liballeg.org")
    (license license:giftware)))

(define-public allegro
  (package
    (name "allegro")
    (version "5.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/allegro/allegro/"
                                  version "/allegro-" version ".tar.gz"))
              (sha256
               (base32
                "1mwzgzc4nb5k5zkbq7yrc6hg63yxq3wk69lmjag1h19x8b6njnmg"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; there are no tests
    (inputs
     ;; FIXME: Add the following optional inputs: xinput2, opensl, dumb
     `(("flac" ,flac)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libxcursor" ,libxcursor)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "http://liballeg.org")
    (license license:bsd-3)))

(define-public allegro-5.0
  (package (inherit allegro)
    (name "allegro")
    (version "5.0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/allegro/allegro/"
                                  version "/allegro-" version ".tar.gz"))
              (sha256
               (base32
                "0cd51qrh97jrr0xdmnivqgwljpmizg8pixsgvc4blqqlaz4i9zj9"))))))

(define-public aseprite
  (package
    (name "aseprite")
    (version "1.1.7") ; After 1.1.7 the source is no longer distributed under the GPL.
    ;; TODO: Unbundle third party software.
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/aseprite/aseprite"
                                  "/releases/download/v" version
                                  "/Aseprite-v" version "-Source.zip"))
              (sha256
               (base32
                "1plss4i1lfxcznv9p0pip1bkhj7ipw7jlhsh5avd6dzw079l4nvv"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       ;; Use shared libraries instead of building bundled source.
       (list "-DWITH_WEBP_SUPPORT=1"
             "-DUSE_SHARED_CURL=1"
             "-DUSE_SHARED_GIFLIB=1"
             "-DUSE_SHARED_JPEGLIB=1"
             "-DUSE_SHARED_ZLIB=1"
             "-DUSE_SHARED_LIBPNG=1"
             "-DUSE_SHARED_LIBLOADPNG=1"
             "-DUSE_SHARED_LIBWEBP=1"
             "-DUSE_SHARED_TINYXML=1"
             "-DUSE_SHARED_PIXMAN=1"
             "-DUSE_SHARED_FREETYPE=1"
             "-DUSE_SHARED_ALLEGRO4=1"
             "-DENABLE_UPDATER=0" ; no auto-updates
             (string-append "-DFREETYPE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; TODO: Use a patched Allegro 4 that supports window resizing.  This
    ;; patched version is bundled with Aseprite, but the patches should be
    ;; extracted and applied on top of a standalone Allegro 4 package.
    (inputs
     `(("allegro" ,allegro-4)
       ("curl" ,curl)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxxf86vm" ,libxxf86vm)
       ("pixman" ,pixman)
       ("tinyxml" ,tinyxml)
       ("zlib" ,zlib)))
    (synopsis "Animated sprite editor and pixel art tool")
    (description "Aseprite is a tool for creating 2D pixel art for video
games.  In addition to basic pixel editing features, Aseprite can assist in
the creation of animations, tiled graphics, texture atlases, and more.")
    (home-page "https://www.aseprite.org/")
    (license license:gpl2+)))

(define-public qqwing
  (package
    (name "qqwing")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://qqwing.com/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bw0papyqjg22z6irf36gs54y8236wa37b6gyn2h1spy65n76lqp"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://qqwing.com/")
    (synopsis "Sudoku puzzle solver and generator")
    (description
     "QQWing is a Sudoku puzzle generator and solver.
It offers the following features:
@enumerate
@item Can solve 1000 puzzles in 1 second and generate 1000 puzzles in 25 seconds.
@item Uses logic.  Uses as many solve techniques as possible when solving
  puzzles rather than guessing.
@item Rates puzzles.  Most generators don't give an indication of the difficulty
  of a Sudoku puzzle.  QQwing does.
@item Can print solve instructions for any puzzle.
@item Customizable output style, including a CSV style that is easy to
  import into a database.
@end enumerate")
    (license license:gpl2+)))

(define-public quesoglc
  (package
    (name "quesoglc")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" version "/"
                                  name "-" version "-free.tar.bz2"))
              (sha256
               (base32
                "08ddhywdy2qg17m592ng3yr0p1ih96irg8wg729g75hsxxq9ipks"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("fontconfig" ,fontconfig)
              ("freeglute" ,freeglut)
              ("fribidi" ,fribidi)
              ("glew" ,glew)))
    (home-page "http://quesoglc.sourceforge.net")
    (synopsis "Implementation of the OpenGL Character Renderer (GLC)")
    (description
     "The OpenGL Character Renderer (GLC) is a state machine that provides
OpenGL programs with character rendering services via an application programming
interface (API).")
    (license (list license:expat license:lgpl2.1+))))

(define-public python-pygame
  (package
    (name "python-pygame")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pygame" version))
              (sha256
               (base32
                "1hlydiyygl444bq5m5g8n3jsxsgrdyxlm42ipmfbw36wkf0j243m"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Tests require pygame to be installed first.
       #:phases
       (modify-phases %standard-phases
         ;; Set the paths to the dependencies manually because
         ;; the configure script does not allow passing them as
         ;; parameters.  This also means we can skip the configure
         ;; phase.
         (add-before 'build 'set-library-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((sdl-ref   (assoc-ref inputs "sdl"))
                   (font-ref  (assoc-ref inputs "sdl-ttf"))
                   (image-ref (assoc-ref inputs "sdl-image"))
                   (mixer-ref (assoc-ref inputs "sdl-mixer"))
                   (smpeg-ref (assoc-ref inputs "libsmpeg"))
                   (png-ref   (assoc-ref inputs "libpng"))
                   (jpeg-ref  (assoc-ref inputs "libjpeg"))
                   (freetype-ref (assoc-ref inputs "freetype"))
                   (v4l-ref   (assoc-ref inputs "v4l-utils"))
                   (out-ref   (assoc-ref outputs "out")))
               (substitute* "Setup.in"
                 (("SDL = -I/usr/include/SDL")
                  (string-append "SDL = -I" sdl-ref "/include/SDL -I.")))
               (substitute* "Setup.in"
                 (("FONT = -lSDL_ttf")
                  (string-append "FONT = -I" font-ref "/include/SDL -L"
                                             font-ref "/lib -lSDL_ttf")))
               (substitute* "Setup.in"
                 (("IMAGE = -lSDL_image")
                  (string-append "IMAGE = -I" image-ref "/include/SDL -L"
                                              image-ref "/lib -lSDL_image")))
               (substitute* "Setup.in"
                 (("MIXER = -lSDL_mixer")
                  (string-append "MIXER = -I" mixer-ref "/include/SDL -L"
                                              mixer-ref "/lib -lSDL_mixer")))
               (substitute* "Setup.in"
                 (("SMPEG = -lsmpeg")
                  (string-append "SMPEG = -I" smpeg-ref "/include/smpeg -L"
                                              smpeg-ref "/lib -lsmpeg")))
               (substitute* "Setup.in"
                 (("PNG = -lpng")
                  (string-append "PNG = -I" png-ref "/include -L"
                                            png-ref "/lib -lpng")))
               (substitute* "Setup.in"
                 (("JPEG = -ljpeg")
                  (string-append "JPEG = -I" jpeg-ref "/include -L"
                                 jpeg-ref "/lib -ljpeg")))

               (substitute* "Setup.in"
                 (("FREETYPE = -lfreetype")
                  (string-append "FREETYPE = -I" freetype-ref "/include/freetype2 -L"
                                 freetype-ref "/lib -lfreetype")))

               (substitute* "Setup.in"
                 (("^pypm") "#pypm"))
               ;; Create a path to a header file provided by v4l-utils.
               (system* "mkdir" "linux")
               (system* "ln" "--symbolic"
                        (string-append v4l-ref "/include/libv4l1-videodev.h")
                        "linux/videodev.h")
               (system* "ln" "--symbolic" "Setup.in" "Setup")))))))
    (inputs
     `(("freetype" ,freetype)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-ttf" ,sdl-ttf)
       ("sdl-gfx" ,sdl-gfx)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libX11" ,libx11)
       ("libsmpeg" ,libsmpeg)
       ("portmidi" ,portmidi)
       ("v4l-utils" ,v4l-utils)))
    (home-page "https://www.pygame.org")
    (synopsis "SDL wrapper for Python")
    (description "Pygame is a set of Python modules designed for writing games.
Pygame adds functionality on top of the excellent SDL library. This allows you
to create fully featured games and multimedia programs in the python language.")
    (license (list license:bsd-2
                   ;; python numeric license as listed by Debian looks like
                   ;; an Expat-style license with a warranty disclaimer for
                   ;; the U.S. government and the University of California.
                   license:expat
                   license:lgpl2.0+
                   license:lgpl2.1+
                   license:gpl3+
                   license:psfl
                   license:public-domain
                   license:lgpl2.1+))))

(define-public python2-pygame
  (package-with-python2 python-pygame))

(define-public grafx2
  (package
    (name "grafx2")
    (version "2.4")
    (source (origin
              (method url-fetch)
              ;; XXX: There is no URL that contains the version. :(
              (uri "http://pulkomandy.tk/projects/GrafX2/downloads/21")
              (sha256
               (base32
                "0svsy6rqmdj11b400c242i2ixihyz0hds0dgicqz6g6dcgmcl62q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-before 'build 'change-to-src-directory
           (lambda _
             (chdir "src")
             #t)))
       #:make-flags
       ;; SDL header files are referenced without the preceeding "SDL/".
       (list (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL")
             (string-append "prefix="
                            (assoc-ref %outputs "out")))
       #:tests? #f)) ; no check target
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-ttf)))))
    (synopsis "Bitmap paint program")
    (description "GrafX2 is a bitmap paint program inspired by the Amiga
programs Deluxe Paint and Brilliance.  Specializing in 256-color drawing, it
includes a very large number of tools and effects that make it particularly
suitable for pixel art, game graphics, and generally any detailed graphics
painted with a mouse.")
    (home-page "http://pulkomandy.tk/projects/GrafX2")
    (license license:gpl2))) ; GPLv2 only
