;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2016-2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2018, 2019, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Julian Graham <joolean@gmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019 Jethro Cao <jethrocao@gmail.com>
;;; Copyright © 2020-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021 Dmitry Polyakov <polyakov@liltechdude.xyz>
;;; Copyright © 2020-2022, 2024-2025 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Andy Tai <atai@atai.org>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 dan <i@dan.games>
;;; Copyright © 2022 Cairn <cairn@pm.me>
;;; Copyright © 2023, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022-2023, 2025 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 宋文武 <iyzsong@envs.net>
;;; Copyright © 2025 Arnaud Lechevallier <arnaud.lechevallier@free.fr>
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
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages squirrel)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public bullet
  (package
    (name "bullet")
    (version "3.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bulletphysics/bullet3/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08xq225zw6z4ic0whaf8xn697vv5lkrdzfkmjvm32biidbjg8qq0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "build3" "premake*"))
                  (with-directory-excursion "examples/ThirdPartyLibs"
                    (for-each delete-file-recursively
                              '("Gwen" "clsocket" "enet" "glad" "imgui"
                                "lua-5.2.3" "midi" "minizip" "openvr"
                                "optionalX11" "serial" "zlib")))

                  ;; Tests fail on linking, cannot find -lBussIK.
                  (substitute* "test/CMakeLists.txt"
                    ((" InverseDynamics")
                     "../examples/ThirdPartyLibs/BussIK InverseDynamics"))))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=ON"
                   "-DBUILD_CPU_DEMOS=OFF"
                   "-DBUILD_OPENGL3_DEMOS=OFF"
                   "-DBUILD_BULLET2_DEMOS=OFF"
                   ;; openmw 0.47.0 requires bullet to be built with
                   ;; double precision.
                   ;; See <https://issues.guix.gnu.org/52953> for
                   ;; more information.
                   "-DUSE_DOUBLE_PRECISION=ON"
                   ;; Extras/BulletRoboticsGUI needs files from
                   ;; ThirdPartyLibs
                   "-DBUILD_BULLET_ROBOTICS_GUI_EXTRA=OFF"
                   ;; Extras/BulletRobotics needs files from
                   ;; ThirdPartyLibs
                   "-DBUILD_BULLET_ROBOTICS_EXTRA=OFF"
                   (string-append  "-DCMAKE_CXX_FLAGS=-fPIC "
                                   (or (getenv "CXXFLAGS") "")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-failing-tests
                 ;; These tests fail specifically after removing 3rd party code.
                 (lambda _
                   (substitute* "test/SharedMemory/CMakeLists.txt"
                     (("ADD_TEST") "# ADD_TEST"))
                   (substitute* "test/InverseDynamics/CMakeLists.txt"
                     (("ADD_TEST\\(Test_BulletInverseForward")
                      "# ADD_TEST(Test_BulletInverseForward")))))))
    (inputs (list glu libx11 mesa))
    (home-page "https://pybullet.org/wordpress/")
    (synopsis "3D physics engine library")
    (description
     "Bullet is a physics engine library usable for collision detection.  It
is used in some video games and movies.")
    (license license:zlib)))

(define-public dds
  (let ((commit "d2bc4c2c703941664fc1d73e69caa5233cdeac18")
        (revision "1"))
    (package
      (name "dds")
      (version (git-version "2.9.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dds-bridge/dds")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ishbb69cvyv96xdxshnly0m5ydwljgdf8fwa1cr9rj2qj40q4rm"))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'chdir
                   (lambda _
                     (chdir "src")))
                 (replace 'configure
                   ;; Configuration is done by copying the appropriate
                   ;; make file in the working directory.  There is no
                   ;; configure script.
                   (lambda _
                     (copy-file "Makefiles/Makefile_linux_shared"
                                "Makefile")))
                 (replace 'check
                   ;; There is no "check" traget.  We must compile
                   ;; a "dtest" program and apply it on a data set.
                   (lambda* (#:key tests? #:allow-other-keys)
                     (when tests?
                       (install-file "libdds.so" "../test")
                       (with-directory-excursion "../test"
                         (copy-file "Makefiles/Makefile_linux"
                                    "Makefile")
                         (substitute* "Makefile"
                           (("-Werror") ""))
                         (invoke "make")
                         (invoke "./dtest" "-f" "../hands/list100.txt")))))
                 (replace 'install
                   ;; "install" target merely moves ".so" file around
                   ;; the source directory.  We install it in the store,
                   ;; along with all shipped documentation (which cannot
                   ;; be built from source unfortunately).
                   (lambda _
                     (install-file "libdds.so"
                                   (string-append #$output "/lib"))
                     (let ((inc (string-append #$output "/include")))
                       (copy-recursively "../include" inc))
                     (let ((doc (string-append #$output
                                               "/share/doc/"
                                               #$name "-" #$version)))
                       (install-file "../LICENSE" doc)
                       (copy-recursively "../doc" doc)))))))
      (native-inputs
       (list gawk procps))
      (inputs
       (list boost))
      (home-page "https://privat.bahnhof.se/wb758135/")
      (synopsis "Double dummy solver for the bridge card game")
      (description "DDS is a double-dummy solver of bridge hands.  It supports
single-threading and multi-threading for improved performance.  DDS
offers a wide range of functions, including par-score calculations.")
      (license license:asl2.0))))

(define-public deutex
  (package
   (name "deutex")
   (version "5.2.2")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/Doom-Utils/deutex"
                          "/releases/download/v" version "/"
                          "deutex-" version ".tar.zst"))
      (sha256
       (base32 "0psb2za6ldrlak7s8pjvli98ij5yiwjx8j1ms2v7rj9yadx0xv8h"))))
   (build-system gnu-build-system)
   (inputs
    (list libpng))
   (native-inputs
    (list asciidoc pkg-config zstd))
   (home-page "https://github.com/Doom-Utils/deutex")
   (synopsis "WAD file composer for Doom and related games")
   (description
    "DeuTex is a wad composer for Doom, Heretic, Hexen and Strife.  It can be
used to extract the lumps of a wad and save them as individual files.
Conversely, it can also build a wad from separate files.  When extracting a
lump to a file, it does not just copy the raw data, it converts it to an
appropriate format (such as PPM for graphics, Sun audio for samples, etc.).
Conversely, when it reads files for inclusion in pwads, it does the necessary
conversions (for example, from PPM to Doom picture format).  In addition,
DeuTex has functions such as merging wads, etc.")
   (license license:gpl2+)))

(define-public go-codeberg-org-anaseto-gruid-sdl
  (package
    (name "go-codeberg-org-anaseto-gruid-sdl")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/anaseto/gruid-sdl.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q2k9ysfvqb715mrpk2f3sagkjmcsinh3s6nfgi6f3axckzj2351"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t ; it needs to be built on final application side only
      #:tests? #f      ; no tests provided
      #:import-path "codeberg.org/anaseto/gruid-sdl"))
    (native-inputs
     (list pkg-config)) ; for go-github-com-veandco-go-sdl2
    (propagated-inputs
     (list go-codeberg-org-anaseto-gruid
           go-github-com-veandco-go-sdl2
           go-golang-org-x-image))
    (home-page "https://codeberg.org/anaseto/gruid-sdl")
    (synopsis "Gruid Driver using the go-sdl2 SDL2 bindings")
    (description
     "Package sdl provides a Driver for making native graphical apps.")
    (license license:isc)))

(define-public go-github-com-veandco-go-sdl2
  (package
    (name "go-github-com-veandco-go-sdl2")
    (version "0.4.40")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/veandco/go-sdl2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wi74j32dj5bzlp85v2qlhxn03p9p3500vxmm3d2wj656nwjw3cg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/veandco/go-sdl2"
      #:test-flags
      #~(list "-skip" (string-join
                       (list
                        ;; QuitSubSystem(32): subsystem still initialized.
                        "TestInitQuit"
                        ;; Field not found "lockData" and type size mismatch.
                        "TestStructABI"
                        ;; Parameter 'src' is invalid.
                        "TestSurface"
                        ;; Test examples is provided as git submodule
                        ;; <https://github.com/veandco/go-sdl2-examples>.
                        "TestTTF")
                       "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sdl-wrapper-headers
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* (find-files "." "\\.h")
                  (("<SDL_image.h>") "<SDL2/SDL_image.h>")
                  (("<SDL_mixer.h>") "<SDL2/SDL_mixer.h>")
                  (("<SDL_ttf.h>") "<SDL2/SDL_ttf.h>")))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "XDG_RUNTIME_DIR" (getcwd)))))))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list go-github-com-golang-freetype
           sdl2
           sdl2-gfx
           sdl2-image
           sdl2-mixer
           sdl2-ttf))
    (home-page "https://github.com/veandco/go-sdl2")
    (synopsis "SDL2 binding for Go")
    (description
     "@code{go-sdl2} is SDL2 wrapped for Go users.  It enables
interoperability between Go and the SDL2 library which is written in C. That
means the original SDL2 installation is required for this to work.")
    (license license:bsd-3)))

(define-public grfcodec
  ;; Latest release 6.0.6 requires an older boost and does not build with our
  ;; newer GCC.
  (let ((commit "7ded8ebd1447bd2e7c0f4b587be0c0510397bdd0")
        (revision "0"))
    (package
      (name "grfcodec")
      (version (git-version "6.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/OpenTTD/grfcodec")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12bf5y7d83plrlssdlcj83w4yxmg5jp1w2p8570l92hy9mkcfmb9"))
         (modules '((guix build utils)))
         (snippet
           `(begin
              ;; The sources are not a git repository
              (substitute* "generate_version.cmake"
                (("\\$\\{GIT.*describe.*") (string-append "echo \"" ,version "\"\n"))
                (("\\$\\{GIT.*show.*") "echo \"Not shown for reproducibility.\"\n"))
              (substitute* "CMakeLists.txt"
                (("find_package\\(Git REQUIRED\\)") ""))))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f                      ;no check target
         #:phases
         (modify-phases %standard-phases
           (replace 'install              ;no install target
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (doc (string-append out "/share/doc"))
                      (man (string-append out "/share/man/man1")))
                 (for-each (lambda (file)
                             (install-file file bin))
                           '("grfcodec" "grfid" "grfstrip" "nforenum"))
                 (with-directory-excursion "../source"
                   (install-file "COPYING" doc)
                   (with-directory-excursion "docs"
                     (for-each (lambda (file)
                                 (install-file (string-append file ".txt") doc))
                               '("auto_correct" "commands" "grf" "grfcodec" "grftut"
                                 "readme" "readme.rpn"))
                     (for-each (lambda (file)
                                 (install-file file man))
                               (find-files "." "\\.1"))))))))))
      (inputs
       (list boost libpng zlib))
      (synopsis "GRF development tools")
      (description
       "The @dfn{Graphics Resource File} (GRF) development tools are a set of
tools for developing (New)GRFs.  It includes a number of smaller programs, each
with a specific task:
@enumerate
@item @code{grfcodec} decodes and encodes GRF files for OpenTTD.
@item @code{grfid} extracts the so-called \"GRF ID\" from a GRF.
@item @code{grfstrip} strips all sprites from a GRF.
@item @code{nforenum} checks NFO code for errors, making corrections when
necessary.
@end enumerate")
      (home-page "https://dev.openttdcoop.org/projects/grfcodec")
      ;; GRFCodec, GRFID, and GRFStrip are exclusively under the GPL2.
      ;; NFORenum is under the GPL2+.
      ;; The MD5 implementation contained in GRFID is under the zlib license.
      (license (list license:gpl2 license:gpl2+ license:zlib)))))

(define-public catcodec
  (package
    (name "catcodec")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://binaries.openttd.org/extra/catcodec/"
                           version "/catcodec-" version "-source.tar.xz"))
       (sha256
        (base32 "1qg0c2i4p29sxj0q6qp2jynlrzm5pphz2xhcjqlxa69ycrnlxzs7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list (string-append "prefix=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://dev.openttdcoop.org/projects/catcodec")
    (synopsis "Encode/decode OpenTTD sounds")
    (description "catcodec encodes and decodes sounds for OpenTTD.  These
sounds are not much more than some metadata (description and filename) and raw
PCM data.")
    (license license:gpl2)))

(define-public gzochi
  (package
    (name "gzochi")
    (version "0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gzochi/gzochi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vcvf04qqzs3q8kaild2x7qvkwc6bwzfsisb78147b8z747j7hj0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'no-Werror
                    (lambda _
                      ;; Don't abort builds due to things like GLib
                      ;; deprecation warnings.
                      (substitute* (find-files "." "^Makefile\\.in$")
                        (("-Werror") ""))
                      #t)))))
    (native-inputs (list pkg-config))
    (inputs (list bdb
                  glib
                  gmp
                  guile-3.0
                  libmicrohttpd
                  ncurses
                  sdl
                  zlib))
    (home-page "https://www.nongnu.org/gzochi/")
    (synopsis "Scalable middleware for multiplayer games")
    (description
     "gzochi is a framework for developing massively multiplayer online games.
A server container provides services to deployed games, which are written in
Guile Scheme, that abstract and simplify some of the most challenging and
error-prone aspects of online game development: Concurrency, data persistence,
and network communications.  A very thin client library can be embedded to
provide connectivity for client applications written in any language.")
    (license license:gpl3+)))

(define-public nml
  (package
    (name "nml")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nml" version))
       (sha256
        (base32 "1kgzkv8pc0blck8c6iqq1idx1nrxyjw2vbnrdisnxizi6zds5l73"))))
    (build-system python-build-system)
    ;; TODO: Fix test that fails with
    ;; "AttributeError: partially initialized module 'nml.nmlop' has no
    ;; attribute 'ADD' (most likely due to a circular import)"
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     (list python-pillow python-ply))
    (home-page "https://github.com/OpenTTD/nml")
    (synopsis "NML compiler")
    (description
     "@dfn{NewGRF Meta Language} (NML) is a python-based compiler, capable of
compiling NML files (along with their associated language, sound and graphic
files) into @file{.grf} and/or @file{.nfo} files.")
    (license license:gpl2+)))

(define-public python-pybox2d
  (package
    (name "python-pybox2d")
    (version "2.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pybox2d/pybox2d")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0dha28yscr1lpyzy9ygqc01a8pyf7n9vavyxikqh469wr2zcacna"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; It is not clear how to run the tests
      #:tests? #false
      #:phases
      '(modify-phases %standard-phases
         (add-before 'build 'build-ext
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs (list swig python-setuptools python-wheel))
    (home-page "https://github.com/pybox2d/pybox2d")
    (synopsis "2D game physics for Python")
    (description
     "Pybox2d is a 2D physics library for your games and simple simulations.
It's based on the Box2D library, written in C++.  It supports several shape
types (circle, polygon, thin line segments), and quite a few joint
types (revolute, prismatic, wheel, etc.).")
    (license license:zlib)))

(define-public python-sge
  (package
    (name "python-sge")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sge" version))
       (sha256
        (base32
         "1bp4spcjsmy6xv2j0ciaripfgd3pj5413hhhrj8v8mxcwhraw68p"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests
    (native-inputs
     (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-pygame python-uniseg))
    (home-page "https://python-sge.github.io/")
    (synopsis "2D game engine for Python")
    (description
     "The SGE Game Engine (\"SGE\", pronounced like \"Sage\") is a
general-purpose 2D game engine.  It takes care of several details for you so
you can focus on the game itself.  This makes more rapid game development
possible, and it also makes the SGE easy to learn.")
    (license license:lgpl3+)))

(define-public python-pyscroll
  (package
    (name "python-pyscroll")
    (version "2.31")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyscroll" version))
              (sha256
               (base32
                "0w3c58mkkbsyvx9w9hwdizk20pbds800m7v9vg49ydw440dha0hr"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pygame))
    (home-page "https://github.com/bitcraft/pyscroll")
    (synopsis "Fast scrolling maps library for pygame")
    (description "@code{pyscroll} is a simple and fast module
for animated scrolling maps for your new or existing game.")
    (license license:lgpl3+)))

(define-public python-pytmx
  (package
    (name "python-pytmx")
    (version "3.32")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyTMX" version))
              (sha256
               (base32
                "1jh9b0pjqbjdv72v5047p5d769ic084g013njvky0zcfiwrxi3w5"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pygame python-pysdl2 python-pyglet))
  (home-page "https://github.com/bitcraft/PyTMX")
  (synopsis "Python library to read Tiled Map Editor's TMX maps")
  (description "@code{pytmx} is a map loader for python/pygame designed for games.
It provides smart tile loading with a fast and efficient storage base.")
  (license license:lgpl3+)))

(define-public python-tmx
  (package
    (name "python-tmx")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/python-tmx/"
                           (version-major+minor version) "/tmx-"
                           version ".tar.gz"))
       (sha256
        (base32
         "073q0prg1nzlkga2b45vhscz374206qh4x68ccg00mxxwagn64z0"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six))
    (home-page "https://python-tmx.nongnu.org")
    (synopsis "Python library for the @code{Tiled} TMX format")
    (description
     "Python TMX reads and writes the @code{Tiled} TMX format in a simple way.
This is useful for map editors or generic level editors, and it's also useful
for using a map editor or generic level editor like Tiled to edit your game's
levels.")
    (license (list license:asl2.0
                   ;; Documentation (only available in the source tarball) is
                   ;; under the CC0 license.
                   license:cc0))))

(define-public python-xsge
  (package
    (name "python-xsge")
    (version "2021.10.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/python-sge/xsge"
                                  "/releases/download/v" version
                                  "/xsge-" version ".tar.gz"))
              (sha256
               (base32
                "0g86p5i9lflvblzy7977lmxbsma5hdlz63sk0k8c59vnhg8sij4i"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; xSGE's setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/"))))
       #:tests? #f)) ; no check target
    (propagated-inputs
     (list python-sge))
    (home-page "https://python-sge.github.io/")
    (synopsis "Extensions for the SGE Game Engine")
    (description
     "xSGE is a collection of modules that make doing certain tasks with the SGE
Game Engine easier.  In addition to SGE's conveniences, the user has access to a
GUI toolkit, lighting and physics frameworks and @code{Tiled} TMX format
support.")
    (license license:lgpl3+)))

(define-public python-neteria
  (package
    (name "python-neteria")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "neteria" version))
       (sha256
        (base32 "1azlix80a6vns2i3z0bdbqk32kx8s2gjh2nvshab235fd9h85yv7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-rsa))
    (home-page "https://pypi.org/project/neteria/")
    (synopsis "Simple game networking library")
    (description
     "This package provides a game networking framework for Python.")
    (license license:gpl3+)))

(define-public slade
  (package
    (name "slade")
    (version "3.2.5a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sirjuddington/SLADE")
             (commit version)))
       (sha256
        (base32 "1pdrw5ysyh9s907gj6bwf16sf9nm89dlnwlpn0y8x49662kx41v3"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DWX_GTK3=ON" "-DNO_WEBVIEW=ON"
                   "-DBUILD_PK3=ON"
                   (string-append "-DWITH_WXPATH="
                                  #$(this-package-input "wxwidgets") "/bin")
                   (string-append "-DwxWidgets_LIBRARIES="
                                  #$(this-package-input "wxwidgets") "/lib"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-with-x11-gdk-backend
                 ;; Set GDK_BACKEND to x11 to prevent crash on Wayland.
                 ;; See https://github.com/sirjuddington/SLADE/issues/1097 for
                 ;; details.
                 (lambda _
                   (wrap-program (string-append #$output "/bin/slade")
                     '("GDK_BACKEND" = ("x11"))))))
           #:tests? #f)) ;; No test suite.
    (inputs
     (list bash-minimal
           curl
           fluidsynth
           freeimage
           ftgl
           glew
           gtk+
           lua
           mpg123
           sfml
           wxwidgets))
    (native-inputs
     (list pkg-config which zip))
    (home-page "https://slade.mancubus.net")
    (synopsis "Doom game data editor")
    (description "SLADE3 is a modern editor for Doom-engine based games and
source ports.  It has the ability to view, modify, and write many different game-
specific formats, and even convert between some of them, or from/to other generic
formats such as PNG.")
    (license license:gpl2+)))

(define-public tiled
  (package
    (name "tiled")
    (version "1.8.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mapeditor/tiled")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iimfj4kbhmjk94586fqz11bk4i7v0zsxby8agx7450cqlh2y3zi"))))
    (build-system gnu-build-system)
    (inputs
     (list qtbase-5 qtdeclarative-5 qtsvg-5 zlib))
    (native-inputs
     (list qttools-5))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "translations/translations.pro"
               (("LRELEASE =.*")
                (string-append "LRELEASE = "
                               (search-input-file inputs "/bin/lrelease")
                               "\n")))
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))

(define-public trenchbroom
  (package
    (name "trenchbroom")
    (version "2024.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TrenchBroom/TrenchBroom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18cb3w7wxc9y2izh0flkkl77sg897dh0g49zq7rbhpvw35j4xgaj"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DCMAKE_BUILD_TYPE=Release" "-G" "Unix Makefiles"
                   "-DCMAKE_PREFIX_PATH=cmake/packages"
                   (string-append "-DFREEIMAGE_INCLUDE_PATH="
                                  #$freeimage "/include")
                   (string-append "-DFREEIMAGE_LIBRARY="
                                  #$freeimage "/lib/libfreeimage.so")
                   (string-append "-Dfreetype_INCLUDE_DIR="
                                  #$freetype "/include/freetype2")
                   (string-append "-Dfreetype_LIBRARY="
                                  #$freetype "/lib/libfreetype.so"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-build-system
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("set\\(CMAKE_TOOLCHAIN_FILE")
                      "#set(CMAKE_TOOLCHAIN_FILE"))
                   (substitute* "app/CMakeLists.txt"
                     (("/usr") #$output))))
               (add-before 'build 'set-environment-variables
                 (lambda _
                   ;; Set home so fontconfig can write cache.
                   (setenv "HOME" (getenv "TEMP"))
                   ;; Set QT platform for offscreen rendering.
                   (setenv "QT_QPA_PLATFORM" "offscreen")
                   (setenv "XDG_RUNTIME_DIR" (getenv "TEMP"))))
               (add-after 'install 'wrap-trenchbroom
                 (lambda _
                   (wrap-program (string-append #$output "/bin/trenchbroom")
                     ;; TrenchBroom needs $XDG_DATA_DIRS set to find game
                     ;; configs.
                     `("XDG_DATA_DIRS" ":" prefix
                       (,(string-append #$output "/share")))
                     ;; TrenchBroom also doesn't work well with Wayland backend.
                     '("QT_QPA_PLATFORM" = ("xcb")))))
               (add-after 'install 'install-desktop-file
                 (lambda _
                   (make-desktop-entry-file
                    (string-append #$output "/share/applications/"
                                   #$(package-name this-package) ".desktop")
                    #:name "TrenchBroom"
                    #:comment #$(package-synopsis this-package)
                    #:exec #$name
                    #:icon #$name
                    #:categories '("Development")
                    #:keywords '("quake" "level" "editor")))))
           #:tests? #f)) ; No tests.
    (inputs
     (list assimp
           bash-minimal
           catch2
           fmt
           freeglut
           freeimage
           freetype
           glew
           glm
           glu
           libxxf86vm
           mesa
           miniz
           qtbase-5
           qtsvg-5
           tinyxml2))
    (native-inputs (list git pandoc python p7zip))
    (home-page "https://kristianduske.com/trenchbroom/")
    (synopsis "Cross-platform level editor for Quake-engine based games")
    (description "TrenchBroom is a cross-platform level editor for
Quake-engine based games.  It supports Quake, Quake 2, Hexen 2, as well as
other games.  TrenchBroom provides many simple and advanced tools to create
complex and interesting levels.")
    (license license:gpl3+)))

(define-public tsukundere
  (package
    (name "tsukundere")
    (version "0.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/lilyp/tsukundere")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lq2rs33s6l6y0hwwkv8pppgq2ki0q5kzj11s90yivi8g8g201af"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 match)
                  (srfi srfi-1)
                  ((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-command
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((scm (lambda (in)
                           (string-append in "/share/guile/site/"
                                          (target-guile-effective-version))))
                    (ccache (lambda (in)
                              (string-append in "/lib/guile/"
                                             (target-guile-effective-version)
                                             "/site-ccache")))
                    (pkgs
                     (cons
                      (assoc-ref outputs "out")
                      (filter-map
                       (match-lambda
                         (("guile" . pkg) pkg)
                         ((label . pkg)
                          (and (string-prefix? "guile-" label) pkg)))
                       inputs))))
               (substitute* "tsukundere.scm"
                 (("exec guile (.*)" _ args)
                  (string-append
                   ;; XXX: Prevent Guile-SDL2 from blowing up by not knowing
                   ;;      where the SDL2 libaries are.
                   "unset LD_LIBRARY_PATH\n"
                   (format #f "export GUILE_LOAD_PATH=\"~@?\"~%"
                           "~{~a~^:~}" (map scm pkgs))
                   (format #f "export GUILE_LOAD_COMPILED_PATH=\"~@?\"~%"
                           "~{~a~^:~}" (map ccache pkgs))
                   "exec "
                   (assoc-ref inputs "guile")
                   "/bin/guile " args)))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("guile" ,guile-3.0)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs
     `(("guile-sdl2" ,guile3.0-sdl2)
       ("guile" ,guile-3.0)
       ("pango" ,pango)
       ("sdl2" ,sdl2)))
    (home-page "https://gitlab.com/lilyp/tsukundere")
    (synopsis "Visual novel engine")
    (description "Tsukundere is a game engine geared heavily towards the
development of visual novels, written on top of Guile-SDL2.  It is still
experimental.")
    (license license:lgpl3+)))

(define-public scummc
  (package
    (name "scummc")
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
     (list #:test-target "test"
           #:tests? #f ; The only tests verify that game checksums match
           #:make-flags
           #~(list "SHOW_WARNINGS=no")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure ; ScummC uses a non-standard configure
                    (lambda* (#:key configure-flags #:allow-other-keys)
                      (apply invoke  "./configure" configure-flags)))
               (replace 'install ; install target is referred to as distrib
                 (lambda _
                   (invoke "make" "distrib"
                           (string-append "DISTRIB=" #$output)))))))
    (inputs
     (list freetype gtk+-2 sdl))
    (native-inputs
     (list bison doxygen libxslt pkg-config))
    (synopsis "SCUMM Compiler")
    (description
     "ScummC is a set of tools allowing to create SCUMM games from scratch.
It is capable of creating games for SCUMM version 6 and partially version 7.")
    (home-page "https://github.com/AlbanBedel/scummc")
    (license license:gpl2+)))

(define-public sfml
  (package
    (name "sfml")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              ;; Do not fetch the archives from
              ;; http://mirror0.sfml-dev.org/files/ because files there seem
              ;; to be changed in place.
              (uri (git-reference
                    (url "https://github.com/SFML/SFML")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0abr8ri2ssfy9ylpgjrr43m6rhrjy03wbj9bn509zqymifvq5pay"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Ensure system libraries are used.
                  (delete-file-recursively "extlibs")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DSFML_INSTALL_PKGCONFIG_FILES=TRUE"
             "-DSFML_OS_PKGCONFIG_DIR=lib/pkgconfig")
       #:tests? #f)) ; no tests
    (native-inputs
     (list pkg-config))
    (inputs
     `(("mesa" ,mesa)
       ("glew" ,glew)
       ("libx11" ,libx11)
       ("xcb-util-image" ,xcb-util-image)
       ("libxrandr" ,libxrandr)
       ("eudev" ,eudev)
       ("libjpeg" ,libjpeg-turbo)
       ("libsndfile" ,libsndfile)
       ("stb-image" ,stb-image)
       ("stb-image-write" ,stb-image-write)))
    (propagated-inputs
     ;; In Requires.private of pkg-config files.
     (list flac freetype libvorbis openal))
    (home-page "https://www.sfml-dev.org")
    (synopsis "Simple and Fast Multimedia Library")
    (description
     "SFML provides a simple interface to the various computer components,
to ease the development of games and multimedia applications.  It is composed
of five modules: system, window, graphics, audio and network.")
    (license license:zlib)))

(define-public csfml
  (package
    (name "csfml")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SFML/CSFML")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wj1p798myyavld2xdhvvflb5h4nf1vgxxzs6nh5qad44vj9b3kb"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DCSFML_BUILD_DOC=TRUE")
           #:tests? #f)) ;no tests
    (native-inputs (list doxygen))
    (inputs (list sfml))
    (synopsis "C bindings for the SFML multimedia library")
    (description
     "CSFML is the official C binding to the SFML libraries.  SFML provides a
simple interface to the various computer components, to ease the development of
games and multimedia applications.  It is composed of five modules: system,
window, graphics, audio and network.")
    (home-page "https://www.sfml-dev.org/download/csfml/")
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
     (list pkg-config desktop-file-utils))
    (inputs
     (list sdl gtk+))
    (synopsis "Simple sound effect generator")
    (description "Sfxr is a tool for quickly generating simple sound effects.
Originally created for use in video game prototypes, it can generate random
sounds from presets such as \"explosion\" or \"powerup\".")
    (home-page "https://www.drpetter.se/project_sfxr.html")
    (license license:expat)))

(define-public surgescript
  (package
    (name "surgescript")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alemart/surgescript")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17k14108hvz329cqnr3g799ksmiv8d710slnghi2wmwswir8s0jd"))))
     (build-system cmake-build-system)
     (arguments
      (list #:tests? #f)) ; there are no tests
     (home-page "https://docs.opensurge2d.org")
     (synopsis "Scripting language for games")
     (description "@code{SurgeScript} is a dynamically typed object-oriented
scripting language designed for games.  Each object is a state machine that
can be customized by attaching other objects.  The language supports automatic
garbage collection and can be extended with plugins.")
    (license license:asl2.0)))

(define-public physfs
  (package
    (name "physfs")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://icculus.org/physfs/downloads/physfs-"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qzqz4r88gvd8m7sh2z5hvqcr0jfr4wb2f77c19xycyn0rigfk9h"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:configure-flags '("-DPHYSFS_BUILD_STATIC=OFF")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-CMakeLists.txt
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ;; XXX: For some reason CMakeLists.txt disables
                        ;; RUNPATH manipulation when the compiler is GCC.
                        (("CMAKE_COMPILER_IS_GNUCC") "FALSE"))
                      #t)))))
    (inputs
     (list zlib))
    (native-inputs
     (list doxygen))
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
    (version "11.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/love2d/love/releases/download/"
                           version "/love-" version "-linux-src.tar.gz"))
       (sha256
        (base32 "0fachzyfl26gwg13l5anfppzljxpmd0pvwpap0lgva8syx1hhvh6"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list devil
           freetype
           libmodplug
           libtheora
           libvorbis
           luajit
           mesa
           mpg123
           openal
           sdl2
           zlib))
    (synopsis "2D game framework for Lua")
    (description "LÖVE is a framework for making 2D games in the Lua
programming language.")
    (home-page "https://love2d.org/")
    (license license:zlib)))

(define-public love-nuklear
  (package
    (name "love-nuklear")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keharriso/love-nuklear/")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              ;; NOTE: the HEAD of the Nuklear git-submodule is at commit
              ;; "adc52d710fe3c87194b99f540c53e82eb75c2521" of Oct 1 2019
              (file-name (git-file-name name version))
              (sha256
               (base32
                "090xp5c975155hd1pa7bdssdlawvygs5s6icdkwbyc8il5kg5kgv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda _
             (substitute* "CMakeLists.txt"
               (("DESTINATION .") "DESTINATION lib/love")))))))
    (inputs
     (list luajit))
    (synopsis "Lightweight immediate mode GUI for LÖVE games")
    (description "LÖVE is a Lua framework for making 2D games.  Nuklear
is a minimal state immediate mode graphical user interface toolkit.  This
package is the Nuklear bindings for LÖVE created by Kevin Harrison.")
    (home-page "https://github.com/keharriso/love-nuklear/")
    (license license:expat)))

(define-public allegro-4
  (package
    (name "allegro")
    (version "4.4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/"
                                  "releases/download/" version "/allegro-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1m6lz35nk07dli26kkwz3wa50jsrxs1kb6w1nj14a911l34xn6gc"))))
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
                "ADDON_LINKAGE SHARED")))))))
    (inputs
     (list glu libpng libvorbis mesa zlib))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "https://liballeg.org")
    (license license:giftware)))

(define-public allegro
  (package
    (name "allegro")
    (version "5.2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/releases"
                                  "/download/" version "/allegro-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "034pmbmbq6jagpp4lhnyjqmf8gcz5fx74d9rknrm7d4wv4cv7qy1"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; there are no tests
    (inputs
     ;; FIXME: Add the following optional inputs: xinput2, opensl, dumb
     `(("flac" ,flac)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("gtk" ,gtk+)
       ("libjpeg" ,libjpeg-turbo)
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
     (list pkg-config))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "https://liballeg.org")
    (license license:bsd-3)))

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
     (list pkg-config))
    ;; TODO: Use a patched Allegro 4 that supports window resizing.  This
    ;; patched version is bundled with Aseprite, but the patches should be
    ;; extracted and applied on top of a standalone Allegro 4 package.
    (inputs
     `(("allegro" ,allegro-4)
       ("curl" ,curl)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
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

(define-public libresprite
  (package
    (name "libresprite")
    (version "1.0")
    ;; TODO: Unbundle third party software.
    ;; - duktape is bundled inside the project but it's hard to unbundle:
    ;;   there are many differences from a version to the next and it is not
    ;;   really designed to work as a shared lib.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LibreSprite/LibreSprite")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0djbjjh21ahlxzh0b0jp4mpfycam8h9157i4wbxkd618fraadhbp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DWITH_WEBP_SUPPORT=1"
             "-DWITH_DESKTOP_INTEGRATION=1")
       ;; Tests are unmaintained
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("googletest" ,googletest)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxxf86dga" ,libxxf86dga)
       ("libxxf86vm" ,libxxf86vm)
       ("lua" ,lua)                     ; Optional
       ("pixman" ,pixman)
       ("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("tinyxml" ,tinyxml)
       ("zlib" ,zlib)))
    (synopsis "Animated sprite editor and pixel art tool")
    (description "LibreSprite is a tool for creating 2D pixel art for video
games.  In addition to basic pixel editing features, it can assist in the
creation of animations, tiled graphics, texture atlases, and more.
LibreSprite is a fork of the latest GPLv2 commit of Aseprite.")
    (home-page "https://libresprite.github.io/")
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
      (list pkg-config))
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
    (native-inputs (list pkg-config))
    (inputs (list fontconfig freeglut fribidi glew))
    (home-page "https://quesoglc.sourceforge.net")
    (synopsis "Implementation of the OpenGL Character Renderer (GLC)")
    (description
     "The OpenGL Character Renderer (GLC) is a state machine that provides
OpenGL programs with character rendering services via an application programming
interface (API).")
    (license (list license:expat license:lgpl2.1+))))

(define-public python-pygame
  (package
    (name "python-pygame")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pygame" version))
              (sha256
               (base32
                "0jn2n70hmgr33yc6xzdi33cs5w7jnmgi44smyxfarrrrsnsrxf61"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                 ; tests require pygame to be installed first
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build-config
            (lambda _
              (substitute* "buildconfig/config_unix.py"
                (("origincdirs = \\[.*\\]")
                 "origincdirs = os.environ['C_INCLUDE_PATH'].split(\":\")")
                (("ORIGLIBDIRS") "LIBRARY_PATH")
                (("incdirs = \\[\\]") "incdirs = origincdirs")
                (("libdirs = \\[\\]") "libdirs = origlibdirs"))))
          (add-after 'unpack 'fix-sdl2-headers
            (lambda _
              (substitute* "buildconfig/config_unix.py"
                (("SDL_ttf.h") "SDL2/SDL_ttf.h")
                (("SDL_image.h") "SDL2/SDL_image.h")
                (("SDL_mixer.h") "SDL2/SDL_mixer.h"))
              (substitute* "src_c/imageext.c"
                (("SDL_image.h") "SDL2/SDL_image.h"))
              (substitute* "src_c/font.h"
                (("SDL_ttf.h") "SDL2/SDL_ttf.h"))
              (substitute* "src_c/mixer.h"
                (("SDL_mixer.h") "SDL2/SDL_mixer.h"))
              (substitute* "src_c/_sdl2/mixer.c"
                (("SDL_mixer.h") "SDL2/SDL_mixer.h")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list freetype
           sdl2
           sdl2-image
           sdl2-mixer
           sdl2-ttf
           sdl2-gfx
           libjpeg-turbo
           libpng
           libx11
           libsmpeg
           portmidi
           v4l-utils))
    (home-page "https://www.pygame.org")
    (synopsis "SDL wrapper for Python")
    (description "Pygame is a set of Python modules designed for writing games.
It adds functionality on top of the SDL library, allowing you to create games
and multimedia programs in the Python language.")
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

(define-public python-pygame-menu
  (package
    (name "python-pygame-menu")
    (version "4.5.1")
    (source
     ;; Tests not included in release.
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ppizarror/pygame-menu")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xd5d6nfkd5bp2zfq77yglp6mz043w28zprfz7savgmph5kvdnfh"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'prepare-test-environment
                 (lambda _
                   (setenv "HOME" (getcwd))))
               (add-before 'check 'skip-certain-tests
                 (lambda _
                   (substitute* "test/test_font.py"
                     (("test_font_argument") "skip_test_font_argument")
                     (("test_system_load") "skip_test_system_load"))
                   (substitute* "test/test_baseimage.py"
                     ;; Tuples differ: (111, 110) != (110, 109)
                     (("test_invalid_image") "skip_test_invalid_image")
                     (("test_scale") "skip_test_scale")))))))
    (propagated-inputs (list python-pygame python-pyperclip
                             python-typing-extensions))
    (native-inputs (list python-nose2 python-setuptools python-wheel))
    (home-page "https://pygame-menu.readthedocs.io")
    (synopsis "Menu for pygame")
    (description
     "Pygame-menu is a python-pygame library for creating menus and GUIs.
It supports several widgets, such as buttons, color inputs, clock objects,
drop selectors, frames, images, labels, selectors, tables, text inputs,
color switches, and many more, with multiple options to customize.")
    (license license:expat)))

(define-public python-pygame-sdl2
  (let ((real-version "2.1.0")
        (renpy-version "8.3.0"))
    (package
      (inherit python-pygame)
      (name "python-pygame-sdl2")
      (version (string-append real-version "+renpy" renpy-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.renpy.org/dl/" renpy-version
                             "/pygame_sdl2-" version ".tar.gz"))
         (sha256 (base32 "1p8a4v3r5vjxhiwxdmqqhkl38zav6c4a6w6v2nixzdhzyfkgk16n"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; drop generated sources
             (delete-file-recursively "gen")
             (delete-file-recursively "gen3")
             (delete-file-recursively "gen-static")))))
      (build-system python-build-system)
      (arguments
       (list
        #:tests? #f               ; tests require pygame to be installed first
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'set-paths 'set-sdl-vars
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "PYGAME_SDL2_CFLAGS"
                        (string-append "-I"
                                       (assoc-ref inputs "sdl-union")
                                       "/include/SDL2 -D_REENTRANT"))
                (setenv "PYGAME_SDL2_LDFLAGS"
                        (string-append "-L"
                                       (assoc-ref inputs "sdl-union")
                                       "/lib -Wl,-rpath,"
                                       (assoc-ref inputs "sdl-union")
                                       "/lib -Wl,--enable-new-dtags -lSDL2")))))))
      (inputs
       (list (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
      (native-inputs
       (list python-cython))
      (home-page "https://www.renpy.org/")
      (synopsis "Reimplementation of the Pygame API using SDL2")
      (description "Pygame_SDL2 reimplements the Pygame API using SDL2,
staying close to the original, but also adding some SDL2-specific features.
While it aims to be used as a drop-in replacement, it appears to be
developed mainly for Ren'py.")
      (license (list license:lgpl2.1 license:zlib)))))

(define-public python-renpy
  (package
    (name "python-renpy")
    (version "8.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.renpy.org/dl/" version
                           "/renpy-" version "-source.tar.bz2"))
       (sha256
        (base32
         "1xb9ixb73nm271frkchrqpf64bcrdvrk3n4281dxzm4k4wj60rwb"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Build without sync service.
            ;; Encryption is only used for enabling this service and requires
            ;; libhydrogen, which doesn't have a public release, so drop it
            ;; as well
            (for-each delete-file
                      '("renpy/encryption.pyx"
                        "renpy/common/00sync.rpy"))
            (substitute* "module/setup.py"
              (("cython\\(\"renpy\\.encryption\"\\)") ""))
            (substitute* "renpy/__init__.py"
              (("import renpy\\.encryption") ""))
            ;; Trust vc_version.py when it comes to detecting whether a
            ;; version is official.
            (substitute* "renpy/__init__.py"
              (("official = official and .*") ""))))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ; Ren'py doesn't seem to package tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "renpy/editor.py"
                (("xdg-open")
                 (string-append (assoc-ref inputs "xdg-utils")
                                "/bin/xdg-open")))))
          (add-after 'set-paths 'set-build-vars
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (setenv "RENPY_CYTHON"
                      (search-input-file (or native-inputs inputs)
                                         "/bin/cython"))
              (setenv "RENPY_DEPS_INSTALL" (string-join (map cdr inputs) ":"))))
          (replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
              ;; The "module" subdirectory contains a python (really cython)
              ;; project, which is built using a script, that is thankfully
              ;; named "setup.py".
              (with-directory-excursion "module"
                (apply (assoc-ref %standard-phases 'build) args))
              ;; The above only builds the cython modules, but nothing else,
              ;; so we do that here.
              (invoke "python" "-m" "compileall" "renpy")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
              ;; Again, we have to wrap the module installation.
              ;; Additionally, we want to install the python code
              ;; (both source and compiled) in the same directory.
              (let* ((out (assoc-ref outputs "out"))
                     (site (string-append "/lib/python"
                                          (python-version
                                           (assoc-ref inputs "python"))
                                          "/site-packages")))
                (with-directory-excursion "module"
                  (apply (assoc-ref %standard-phases 'install) args))
                (copy-recursively "renpy"
                                  (string-append out site "/renpy"))
                (delete-file-recursively (string-append out site
                                                        "/renpy/common"))))))))
    (native-inputs (list python-cython))
    (inputs
     (list ffmpeg
           freetype
           fribidi
           glew
           libpng
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
           xdg-utils))
    (propagated-inputs (list python-ecdsa python-future python-pygame-sdl2))
    (home-page "https://www.renpy.org/")
    (synopsis "Ren'py python module")
    (description "This package contains the shared libraries and Python modules
of Ren'py.  While functional, they are not meaningful on their own without
the launcher and common Ren'py code provided by the @code{renpy} package and
are only used to bootstrap it.")
    (license license:expat)))

(define-public renpy
  (package
    (inherit python-renpy)
    (name "renpy")
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f ; see python-renpy
      #:modules '((srfi srfi-1)
                  (guix build python-build-system)
                  (guix build utils))
      #:imported-modules `((srfi srfi-1) ,@%python-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-commands
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "launcher/game/choose_directory.rpy"
                (("/usr/bin/python")
                 (search-input-file inputs "/bin/python3")))
              (substitute* "launcher/game/front_page.rpy"
                (("xdg-open")
                 (search-input-file inputs "/bin/xdg-open")))
              (substitute* "launcher/game/project.rpy"
                (("cmd = \\[ executable, \"-EO\", sys.argv\\[0\\] \\]")
                 (string-append "cmd = [ \"" (assoc-ref outputs "out")
                                "/bin/renpy\" ]"))
                ;; Projects are still created in the usual style, so we need
                ;; to adjust the path.
                (("cmd.append\\(self.path\\)")
                 "cmd.append(self.path + \"/game\")"))))
          (add-after 'unpack 'drop-game-from-paths
            (lambda _
              (substitute* (list "launcher/game/gui7.rpy"
                                 "launcher/game/gui7/images.py")
                ((", \"game\", \"gui7\",") ", \"gui7\","))
              #t))
          (add-before 'build 'start-xserver
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((Xvfb (search-input-file (or native-inputs inputs)
                                             "/bin/Xvfb")))
                (setenv "HOME" (getcwd))
                (system (format #f "~a :1 &" Xvfb))
                (setenv "DISPLAY" ":1"))))
          (replace 'build
            (lambda _
              (invoke "python" "renpy.py" "launcher" "quit")
              (invoke "python" "renpy.py" "the_question" "quit")
              (invoke "python" "renpy.py" "tutorial" "quit")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Here we install our custom renpy program.
              ;; After finishing this step, "out" will have the following:
              ;; |-- bin/renpy
              ;; `-- share/renpy ; i.e. path_to_renpy_base()
              ;;     |-- common
              ;;     `-- gui
              ;;
              ;; Note that common shares the source files that would be installed
              ;; by python2-renpy (which are instead deleted from that package),
              ;; but also contains their byte-compiled versions.
              ;; On other systems, renpy_base would point to site-packages or
              ;; even somewhere in /opt.
              ;; The former approach is not as straightforward as it seems
              ;; -- it causes renpy to load files twice for some weird reason --
              ;; and the latter is impossible on Guix. Hence the detour through
              ;; share/renpy and the custom renpy program.
              ;;
              ;; As a convention, other games should be installed as
              ;; subdirectories of share/renpy in their respective outputs as
              ;; well. This differs from the traditional layout, which is
              ;; roughly the following:
              ;; `-- Super Awesome Game
              ;;     |-- game       ; <- the folder we actually want
              ;;     |-- lib        ; compiled renpy module and dependencies
              ;;     |-- renpy      ; yet another copy of Ren'py's code
              ;;     |   |-- common ; the common folder from above
              ;;     |   `-- ...    ; Python code (source + compiled)
              ;;     |-- Super Awesome Game.py
              ;;     `-- Super Awesome Game.sh
              (let* ((out (assoc-ref outputs "out"))
                     (bin/renpy (string-append out "/bin/renpy")))
                (copy-recursively "renpy/common"
                                  (string-append out "/share/renpy/common"))
                (copy-recursively "gui"
                                  (string-append out "/share/renpy/gui"))
                (copy-recursively "sdk-fonts"
                                  (string-append out "/share/renpy/sdk-fonts"))

                (mkdir-p (string-append out "/bin"))
                (copy-file #$(local-file (search-auxiliary-file "renpy/renpy.in"))
                           bin/renpy)
                (substitute* bin/renpy
                  (("@PYTHON@") (search-input-file inputs "bin/python3"))
                  (("@RENPY_BASE@") (string-append out "/share/renpy")))
                (chmod bin/renpy #o755))))

          (add-after 'install 'install-games
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define renpy (assoc-ref outputs "out"))
              ;; TODO: We should offer a renpy-build-system to make the
              ;; installation of Ren'py games easier.
              (define* (install-renpy-game #:key output game name (renpy renpy)
                                           #:allow-other-keys)
                (let* ((name (or name (basename game)))
                       (launcher (string-append output "/bin/renpy-" name))
                       (share (string-append output "/share/renpy/" name)))
                  (copy-recursively (string-append game "/game") share)
                  (mkdir-p (string-append output "/bin"))
                  (with-output-to-file launcher
                    (lambda ()
                      (format #t
                              "#!~a~%~a ~a \"$@\""
                              (search-input-file inputs "/bin/bash")
                              (string-append renpy "/bin/renpy")
                              share)))
                  (chmod launcher #o755)))

              (install-renpy-game #:output (assoc-ref outputs "out")
                                  #:game "launcher")

              (install-renpy-game #:output (assoc-ref outputs "the-question")
                                  #:game "the_question"
                                  #:name "the-question")

              (install-renpy-game #:output (assoc-ref outputs "tutorial")
                                  #:game "tutorial")))
          (replace 'wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (site (string-append "/lib/python"
                                         (python-version
                                          (assoc-ref inputs "python"))
                                         "/site-packages")))
                (wrap-program (string-append out "/bin/renpy")
                  `("GUIX_PYTHONPATH" =
                    (,@(delete-duplicates
                        (map
                         (lambda (store-path)
                           (string-append store-path site))
                         (cons (assoc-ref outputs "out")
                               (map cdr
                                    (filter
                                     (lambda (input)
                                       (string-prefix? "python" (car input)))
                                     inputs))))))))))))))
    (inputs (list bash-minimal
                  python
                  python-pefile
                  python-requests
                  python-renpy
                  python-six
                  `(,python "tk")
                  xdg-utils))
    (propagated-inputs '())
    (native-inputs (list xorg-server-for-tests))
    (outputs (list "out" "tutorial" "the-question"))
    (home-page "https://www.renpy.org/")
    (synopsis "Visual Novel Engine")
    (description "Ren'Py is a visual novel engine that helps you use words,
images, and sounds to tell interactive stories that run on computers and
mobile devices.  These can be both visual novels and life simulation games.
The easy to learn script language allows anyone to efficiently write large
visual novels, while its Python scripting is enough for complex simulation
games.")
    (license license:expat)))

(define-public python-pyxel
  ;; Note to updaters: Use commit and revision even if you're bumping
  ;; to a release, as upstream is known to "reuse" tags.
  ;; See <https://bugs.gnu.org/66015> for more information.
  (let ((commit "be75b724cae9e10e56a82a5421f9dd65390f1a06")
        (revision "2"))
    (package
      (name "python-pyxel")
      ;; This is the latest version to not require Rust…
      (version (git-version "1.4.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/kitao/pyxel")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "03ch79cmh9fxvq6c2f3zc2snzczhqi2n01f254lsigckc7d5wz08"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (substitute* "pyxel/__init__.py"
                (("from collections import MutableSequence")
                 "from collections.abc import MutableSequence"))))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; "Tests" are actually example programs that never halt.
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-build-files
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "setup.py"
                 (("\"pyxel\\.core\\.bin\\.(.*)\"," all arch)
                  (if (string=? arch "linux")
                      all
                      "")))
               (substitute* "pyxel/core/Makefile"
                 (("`sdl2-config")
                  (string-append "`sdl2-config --prefix="
                                 (assoc-ref inputs "sdl2"))))))
           (add-before 'build 'prebuild
             (lambda _
               (invoke "make" "-C" "pyxel/core"))))))
      (inputs
       `(("gifsicle" ,gifsicle)
         ("sdl2" ,(sdl-union (list sdl2 sdl2-image)))))
      (home-page "https://github.com/kitao/pyxel")
      (synopsis "Retro game engine for Python")
      (description "Pyxel is a game engine inspired by retro gaming consoles.
It has a fixed 16-color palette, can hold up to 3 image banks and 8 tilemaps
(256x256 pixels each) and 4 sound channels with 64 definable sounds.  It
also comes with a built-in image and sound editor.")
      (license license:expat))))

(define-public grafx2
  (let ((3rd/6502                       ;GPLv3+, used in source form
         (origin
           (method url-fetch)
           (uri "https://github.com/redcode/6502/releases/download/v0.1/6502-v0.1.tar.xz")
           (sha256
            (base32 "03wlndlmfsz51x7hmrfs02r3fzqk8a0grbzm2h80pm33f4r0z9dv"))))
        (3rd/recoil                     ;GPLv2+, does not install a library
         (origin
           (method url-fetch)
           (uri "https://downloads.sourceforge.net/project/recoil/recoil/6.4.2/recoil-6.4.2.tar.gz")
           (sha256
            (base32 "1p73cgfacia2gxvswhdixk6grpp9rs2n5258axh5vdb6ly8w3pi3")))))
    (package
      (name "grafx2")
      (version "2.9")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/GrafX2/grafX2")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0rf85pm40nmp9f95adbzzfx2ypvqjl51wqvk461c4bk8z7anlniz"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (mkdir "3rdparty/archives")
              (copy-file #$3rd/6502
                         "3rdparty/archives/6502-v0.1.tar.xz")
              (copy-file #$3rd/recoil
                         "3rdparty/archives/recoil-6.4.2.tar.gz")))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)          ; no configure script
            (add-before 'build 'change-to-src-directory
              (lambda _
                (chdir "src"))))
        #:make-flags
        #~(list "API=sdl2"
                (string-append "PREFIX="
                               (assoc-ref %outputs "out")))
        #:tests? #f))                  ; no check target
      (native-inputs
       (list pkg-config which))
      (inputs
       (list fontconfig lua (sdl-union (list sdl2 sdl2-image sdl2-ttf))))
      (synopsis "Bitmap paint program")
      (description "GrafX2 is a bitmap paint program inspired by the Amiga
programs Deluxe Paint and Brilliance.  Specializing in 256-color drawing, it
includes a very large number of tools and effects that make it particularly
suitable for pixel art, game graphics, and generally any detailed graphics
painted with a mouse.")
      (home-page "http://pulkomandy.tk/projects/GrafX2")
      (license license:gpl2)))) ; GPLv2 only

(define-public ois
  (package
    (name "ois")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wgois/OIS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nkh0zrsbyv47c0i0vhdna3jsnvs69pb1svg75avxw6z7kwskgla"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (inputs
     (list libx11))
    (synopsis "Object Oriented Input System")
    (description
     "Cross Platform Object Oriented Input Lib System is a cross platform,
simple solution for using all kinds of Input Devices (Keyboards, Mice,
Joysticks, etc) and feedback devices (e.g. force feedback).  Meant to be very
robust and compatible with many systems and operating systems.")
    (home-page "https://github.com/wgois/OIS")
    (license license:zlib)))

(define-public mygui
  (package
    (name "mygui")
    (version "3.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MyGUI/mygui")
             (commit (string-append "MyGUI" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nayw5shm5nly9bjp0g372kg5ia64dvn6mrmi1c6mdg0n6vgs9xa"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test target
      #:configure-flags
      #~(list "-DMYGUI_INSTALL_DOCS=TRUE"
              ;; Demos and tools are Windows-specific:
              ;; https://github.com/MyGUI/mygui/issues/24.
              "-DMYGUI_BUILD_DEMOS=FALSE"
              "-DMYGUI_BUILD_TOOLS=FALSE")))
    (native-inputs
     (list boost
           doxygen
           pkg-config))
    (inputs
     (list font-dejavu
           freetype
           graphviz
           libx11
           ogre
           ois))
    (synopsis "Fast, flexible and simple GUI")
    (description
     "MyGUI is a library for creating Graphical User Interfaces (GUIs) for games
and 3D applications.  The main goals of MyGUI are: speed, flexibility and ease
of use.")
    (home-page "http://mygui.info/")
    (license license:expat)))

(define-public mygui-gl
  ;; Closure size is reduced by some 800 MiB.
  (package/inherit mygui
    (name "mygui-gl")
    (arguments
     (substitute-keyword-arguments (package-arguments mygui)
       ((#:configure-flags _)
        `(cons* "-DMYGUI_RENDERSYSTEM=4" ; 3 is Ogre, 4 is OpenGL.
                ;; We can't reuse the flags because of the mention to Ogre.
                (list "-DMYGUI_INSTALL_DOCS=TRUE"
                      ;; Demos and tools are Windows-specific:
                      ;; https://github.com/MyGUI/mygui/issues/24.
                      "-DMYGUI_BUILD_DEMOS=FALSE"
                      "-DMYGUI_BUILD_TOOLS=FALSE")))))
    (inputs
     (modify-inputs (package-inputs mygui)
       (delete "ogre")
       (prepend glu
                libglvnd                ; for find_package(… GLX)
                mesa                    ; for find_package(… OpenGL …)
                (sdl-union (list sdl2 sdl2-image)))))
    (synopsis "Fast, flexible and simple GUI (OpenGL backend)")))

(define-public openmw
  (package
    (name "openmw")
    (version "0.48.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenMW/openmw")
             (commit (string-append "openmw-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0amkxfylk1l67d2igihnhhql62xr89wvg1sxbq2rnhczf6vxaj6f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;No test target
       #:configure-flags (list "-DDESIRED_QT_VERSION=5"
                               "-DOPENMW_USE_SYSTEM_RECASTNAVIGATION=ON")))
    (native-inputs (list boost doxygen pkg-config))
    (inputs (list bullet
                  ffmpeg
                  libxt
                  lz4
                  mygui-gl ;OpenMW does not need Ogre.
                  openal
                  openmw-openscenegraph
                  qtbase-5
                  recastnavigation
                  sdl2
                  unshield
                  icu4c
                  yaml-cpp
                  luajit
                  sqlite))
    (synopsis "Re-implementation of the RPG Morrowind engine")
    (description
     "OpenMW is a game engine which reimplements and extends the one that runs
the 2002 open-world RPG Morrowind.  The engine comes with its own editor,
called OpenMW-CS which allows the user to edit or create their own original
games.")
    (home-page "https://openmw.org")
    (license license:gpl3)))

(define-public godot-lts
  (package
    (name "godot")
    (version "3.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godotengine/godot")
                    (commit (string-append version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vdpd05i901p69ciagdaiwdw21j65a7s9r96gdzay321a0xihr71"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               '(begin
                  ;; Keep only those bundled files we have not (yet) replaced
                  ;; with Guix versions. Note that some of these may be
                  ;; modified; see "thirdparty/README.md".
                  (with-directory-excursion "thirdparty"
                    (let* ((preserved-files
                            '("README.md"
                              "certs"
                              "cvtt"
                              "embree"
                              "enet"
                              "etc2comp"
                              "fonts"
                              "glad"
                              "jpeg-compressor"
                              "libsimplewebm"
                              "minimp3"
                              "miniupnpc"
                              "minizip"
                              "misc"
                              "nanosvg"
                              "oidn"
                              "pvrtccompressor"
                              "recastnavigation"
                              "rvo2"
                              "squish"
                              "stb_rect_pack"
                              "tinyexr"
                              "vhacd"
                              "xatlas")))
                      (for-each delete-file-recursively
                                (lset-difference
                                 string=?
                                 (scandir ".")
                                 (cons* "." ".." preserved-files)))))))))
    (build-system scons-build-system)
    (arguments
     (list
      ;; Avoid using many of the bundled libs.
      ;; Note: These options can be found in the SConstruct file.
      #:scons-flags #~(list "platform=x11" "target=release_debug"
                            "builtin_bullet=no"
                            "builtin_freetype=no"
                            "builtin_glew=no"
                            "builtin_libmpdec=no"
                            "builtin_libogg=no"
                            "builtin_libpng=no"
                            "builtin_libtheora=no"
                            "builtin_libvorbis=no"
                            "builtin_libvpx=no"
                            "builtin_libwebp=no"
                            "builtin_mbedtls=no"
                            "builtin_opus=no"
                            "builtin_pcre2=no"
                            "builtin_wslay=no"
                            "builtin_zlib=no"
                            "builtin_zstd=no")
      #:tests? #f                      ; There are no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'scons-use-env
            (lambda _
              ;; Scons does not use the environment variables by default,
              ;; but this substitution makes it do so.
              (substitute* "SConstruct"
                (("env_base = Environment\\(tools=custom_tools\\)")
                 (string-append
                  "env_base = Environment(tools=custom_tools)\n"
                  "env_base = Environment(ENV=os.environ)")))))
          ;; Build headless tools, to package games without depending on X.
          (add-after 'build 'build-headless
            (lambda* (#:key scons-flags #:allow-other-keys)
              (apply invoke "scons"
                     `(,(string-append
                         "-j" (number->string (parallel-job-count)))
                       "platform=server"
                       ,@(delete "platform=x11" scons-flags)))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (headless (assoc-ref outputs "headless"))
                     (zenity (assoc-ref inputs "zenity")))
                ;; Strip build info from filenames.
                (with-directory-excursion "bin"
                  (for-each
                   (lambda (file)
                     (let ((dest (car (string-split (basename file) #\.))))
                       (rename-file file dest)))
                   (find-files "." "godot.*\\.x11\\.opt\\.tools.*"))
                  (install-file "godot" (string-append out "/bin"))
                  (install-file "godot_server"
                                (string-append headless "/bin")))
                ;; Tell the editor where to find zenity for OS.alert().
                (wrap-program (string-append out "/bin/godot")
                  `("PATH" ":" prefix (,(string-append zenity "/bin")))))))
          (add-after 'install 'wrap-ld-path
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (pulseaudio_path (string-append
                                       (assoc-ref inputs "pulseaudio") "/lib"))
                     (alas_lib_path (string-append
                                     (assoc-ref inputs "alsa-lib") "/lib")))
                (wrap-program (string-append out "/bin/godot")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,pulseaudio_path ,alas_lib_path))))))
          (add-after 'install 'install-godot-desktop
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (applications (string-append out "/share/applications"))
                     (icons (string-append out "/share/icons/hicolor")))
                (mkdir-p applications)
                (copy-file "misc/dist/linux/org.godotengine.Godot.desktop"
                           (string-append applications "/godot.desktop"))
                (for-each (lambda (icon dest)
                            (mkdir-p (dirname dest))
                            (copy-file icon dest))
                          '("icon.png" "icon.svg")
                          `(,(string-append icons "/256x256/apps/godot.png")
                            ,(string-append icons
                                            "/scalable/apps/godot.svg")))))))))
    (outputs '("out" "headless"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           bash-minimal
           bullet
           freetype-with-brotli
           glew
           glu
           libtheora
           libvorbis
           libvpx
           libwebp
           libx11
           libxcursor
           libxi
           libxinerama
           libxrandr
           mbedtls-lts
           mesa
           opusfile
           pcre2
           pulseaudio
           eudev
           wslay
           zenity
           `(,zstd "lib")))
    (home-page "https://godotengine.org/")
    (synopsis "Advanced 2D and 3D game engine")
    (description
     "Godot is an advanced multi-platform game engine written in C++.  If
features design tools such as a visual editor, can import 3D models and
provide high-quality 3D rendering, it contains an animation editor, and can be
scripted in a Python-like language.")
    (license license:expat)))

(define-public godot
  (package
    (name "godot")
    (version "4.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godotengine/godot")
                    (commit (string-append version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fdq69jisrvihmdir2pg6wf4mfqgqg3c0szc58mgci2lqlm4l684"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               '(begin
                  ;; Keep only those bundled files we have not (yet) replaced
                  ;; with Guix versions. Note that some of these may be
                  ;; modified; see "thirdparty/README.md".
                  (with-directory-excursion "thirdparty"
                    (let* ((preserved-files
                            '("README.md"
                              "amd-fsr"
                              "amd-fsr2"
                              "assimp"
                              "astcenc"
                              "basis_universal"
                              ;; Godot needs ca-certificates.crt, but that is
                              ;; not available in build environment
                              "certs"
                              "clipper2"
                              "cvtt"
                              "linuxbsd_headers"
                              "etc2comp"
                              "etcpak"
                              "fonts"
                              "glad"
                              ;; TODO: Remove once Godot once again builds
                              ;; with our glslang package, or with a
                              ;; workaround.  Currently it looks for a Types.h
                              ;; which is no longer in the glslang output
                              ;; after the most recent update.
                              "glslang"
                              "jolt_physics"
                              "jpeg-compressor"
                              "libktx"
                              "libsimplewebm"
                              "manifold"
                              "meshoptimizer"
                              "minimp3"
                              "miniupnpc"
                              "minizip"
                              "misc"
                              "msdfgen"
                              "nanosvg"
                              "noise"
                              "oidn"
                              "openxr"
                              "pvrtccompressor"
                              "recastnavigation"
                              "rvo2"
                              "spirv-reflect"
                              "squish"
                              "stb_rect_pack"
                              "thorvg"
                              "tinyexr"
                              "ufbx"
                              "vhacd"
                              "volk"
                              "vulkan"
                              "xatlas")))
                      (for-each delete-file-recursively
                                (lset-difference string=?
                                                 (scandir ".")
                                                 (cons* "." ".." preserved-files)))))))))
    (build-system scons-build-system)
    (arguments
     (list
      #:scons-flags #~`("platform=linuxbsd" "target=editor" "production=yes"
                        ;; XXX: There may be advantages to enabling volk,
                        ;; requiring unbundling and patching to use our input.
                        "use_volk=no"
                        ;; Avoid using many of the bundled libs.
                        ;; Note: These options can be found in the SConstruct file.
                        "builtin_brotli=no"
                        "builtin_embree=no"
                        "builtin_enet=no"
                        "builtin_freetype=no"
                        ;; TODO: Uncomment this option when the todo for
                        ;; glslang in the snippet is resolved.
                        ;; "builtin_glslang=no"
                        "builtin_graphite=no"
                        "builtin_harfbuzz=no"
                        "builtin_icu4c=no"
                        "builtin_libogg=no"
                        "builtin_libpng=no"
                        "builtin_libtheora=no"
                        "builtin_libvorbis=no"
                        "builtin_libwebp=no"
                        "builtin_mbedtls=no"
                        "builtin_pcre2=no"
                        "builtin_pcre2_with_jit=no"
                        "builtin_wslay=no"
                        "builtin_zlib=no"
                        "builtin_zstd=no")
      #:tests? #f                      ; There are no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'scons-use-env
            (lambda _
              ;; Scons does not use the environment variables by default,
              ;; but this substitution makes it do so.
              (substitute* "SConstruct"
                (("env = Environment\\(tools=\\[\\]\\)")
                 (string-append
                  "env = Environment(tools=[])\n"
                  "env = Environment(ENV=os.environ)")))))
          (add-after 'scons-use-env 'fix-dlopen-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((files '("drivers/alsa/asound-so_wrap.c"
                             "drivers/pulseaudio/pulse-so_wrap.c"
                             "platform/linuxbsd/dbus-so_wrap.c"
                             "platform/linuxbsd/fontconfig-so_wrap.c"
                             "platform/linuxbsd/libudev-so_wrap.c"
                             "platform/linuxbsd/speechd-so_wrap.c"
                             "platform/linuxbsd/wayland/dynwrappers/libdecor-so_wrap.c"
                             "platform/linuxbsd/wayland/dynwrappers/wayland-client-core-so_wrap.c"
                             "platform/linuxbsd/wayland/dynwrappers/wayland-cursor-so_wrap.c"
                             "platform/linuxbsd/wayland/dynwrappers/wayland-egl-core-so_wrap.c"
                             "platform/linuxbsd/x11/display_server_x11.cpp"
                             "platform/linuxbsd/x11/dynwrappers/xcursor-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xext-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xinerama-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xinput2-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xlib-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xrandr-so_wrap.c"
                             "platform/linuxbsd/x11/dynwrappers/xrender-so_wrap.c"
                             "platform/linuxbsd/xkbcommon-so_wrap.c"
                             "thirdparty/volk/volk.c"
                             "thirdparty/volk/volk.c"))
                    (libs '("libasound.so.2"
                            "libpulse.so.0"
                            "libdbus-1.so.3"
                            "libfontconfig.so.1"
                            "libudev.so.1"
                            "libspeechd.so.2"
                            "libdecor-0.so.0"
                            "libwayland-client.so.0"
                            "libwayland-cursor.so.0"
                            "libwayland-egl.so.1"
                            "libXrandr.so.2"
                            "libXcursor.so.1"
                            "libXext.so.6"
                            "libXinerama.so.1"
                            "libXi.so.6"
                            "libX11.so.6"
                            "libXrandr.so.2"
                            "libXrender.so.1"
                            "libxkbcommon.so.0"
                            "libvulkan.so.1"
                            "libvulkan.so")))
                (for-each (lambda (file lib)
                            (substitute* file
                              (((string-append "dlopen\\(\"" lib "\""))
                               (string-append "dlopen(\""
                                              (search-input-file
                                               inputs (string-append "lib/" lib))
                                              "\""))))
                          files libs))
              (substitute* "thirdparty/glad/gl.c"
                (("libGL.so") ; for both .so and .so.1
                 (string-append (search-input-file inputs "lib/libGL.so"))))
              (substitute* "thirdparty/glad/glx.c"
                (("libGL.so") ; for both .so and .so.1
                 (string-append (search-input-file inputs "lib/libGL.so"))))))
          (add-after 'fix-dlopen-paths 'unbundle-xkbcommon
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "platform/linuxbsd/xkbcommon-so_wrap.c"
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon.h")))
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon-compose.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon-compose.h")))
                (("./thirdparty/linuxbsd_headers/xkbcommon/xkbcommon-keysyms.h")
                 (string-append
                  (search-input-file inputs "include/xkbcommon/xkbcommon-keysyms.h"))))))
          (add-after 'unbundle-xkbcommon 'unbundle-wayland
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "platform/linuxbsd/wayland/SCsub"
                ;; This first file does not exist in a "protocol" directory of
                ;; our wayland package, so this can't be grouped with the
                ;; other substitutions.
                (("#thirdparty/wayland/protocol/wayland.xml")
                 (search-input-file inputs "share/wayland/wayland.xml"))
                (("#thirdparty/wayland-protocols")
                 (string-append
                  #$(this-package-input "wayland-protocols") "/share/wayland-protocols"))
                (("#thirdparty/wayland")
                 (string-append
                    #$(this-package-input "wayland") "/share/wayland")))))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((zenity (search-input-file inputs "bin/zenity")))
                ;; Strip build info from filenames.
                (with-directory-excursion "bin"
                  (for-each
                   (lambda (file)
                     (let ((dest (car (string-split (basename file) #\.))))
                       (rename-file file dest)))
                   (find-files "." "godot.*\\.linuxbsd\\.editor.*"))
                  (install-file "godot" (string-append #$output "/bin")))
                ;; Tell the editor where to find zenity for OS.alert().
                ;; TODO: This could be changed in
                ;; platform/linuxbsd/os_linuxbsd.cpp directly, along with the
                ;; other alert programs.
                (wrap-program (string-append #$output "/bin/godot")
                  `("PATH" ":" prefix (,(string-append zenity "/bin")))))))
          (add-after 'install 'install-godot-desktop
            (lambda _
              (let ((applications (string-append #$output "/share/applications"))
                     (icons (string-append #$output "/share/icons/hicolor")))
                (mkdir-p applications)
                (copy-file "misc/dist/linux/org.godotengine.Godot.desktop"
                           (string-append applications "/godot.desktop"))
                (for-each (lambda (icon dest)
                            (mkdir-p (dirname dest))
                            (copy-file icon dest))
                          '("icon.png" "icon.svg")
                           `(,(string-append icons "/256x256/apps/godot.png")
                             ,(string-append icons "/scalable/apps/godot.svg")))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           bash-minimal
           brotli
           dbus
           embree
           enet
           eudev
           fontconfig
           freetype-with-brotli
           glew
           glslang
           glu
           libpng
           harfbuzz
           icu4c
           libdecor
           libtheora
           libvorbis
           libvpx
           libwebp
           libx11
           libxcursor
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           mbedtls-lts
           mesa
           openxr
           opusfile
           pcre2
           pulseaudio
           speech-dispatcher
           vulkan-loader
           wayland
           wayland-protocols
           wslay
           zenity
           zlib
           `(,zstd "lib")))
    (home-page "https://godotengine.org/")
    (synopsis "Advanced 2D and 3D game engine")
    (description
     "Godot is an advanced multi-platform game engine written in C++.  If
features design tools such as a visual editor, can import 3D models and
provide high-quality 3D rendering, it contains an animation editor, and can be
scripted in a Python-like language.")
    (license license:expat)))

(define-public entt
  (package
    (name "entt")
    (version "3.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/skypjack/entt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "151jg3m262xwaywl2rqnc90yr6p48rhmgi5mxyv6bwqvmfli2m5p"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DENTT_BUILD_TESTING=ON"
                               "-DENTT_FIND_GTEST_PACKAGE=ON"
                               "-DENTT_BUILD_DOCS=ON")
       ;; Only tests are compiled, and they need assertions to work correctly.
       #:build-type "Debug"))
    (native-inputs
     (list ;; for testing
           googletest
           ;; for documentation
           doxygen graphviz))
    (synopsis "Entity component system")
    (description "EnTT is a header-only library, containing (among other things)
@itemize
@item an entity component system based on sparse sets,
@item a configuration system using the monostate pattern,
@item a static reflection system,
@item and a cooperative scheduler.
@end itemize")
    (home-page "https://github.com/skypjack/entt")
    (license (list license:expat        ; code
                   license:cc-by4.0)))) ; documentation

(define-public ericw-tools
  (package
    (name "ericw-tools")
    (version "0.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/ericwa/ericw-tools")
                           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11sap7qv0rlhw8q25azvhgjcwiql3zam09q0gim3i04cg6fkh0vp"))
       (patches
        (search-patches "ericw-tools-add-check-for-sse2-in-light.cc.patch"
                        "ericw-tools-gcc-11-pass-const-to-offsetof.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DENABLE_LIGHTPREVIEW=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-copying-embree-files
                 (lambda _
                   ;; Tries to copy files from embree, disable it.
                   (substitute* "light/CMakeLists.txt"
                     (("install\\\(FILES \\$\\{EMBREE")
                      "#install(FILES ${EMBREE"))))
               (add-after 'install 'rename-binaries
                 (lambda _
                   ;; Rename binaries to prevent collisions with other
                   ;; packages.
                   (rename-file (string-append #$output "/bin/bspinfo")
                                (string-append #$output "/bin/qbspinfo"))
                   (rename-file (string-append #$output "/bin/bsputil")
                                (string-append #$output "/bin/qbsputil"))
                   (rename-file (string-append #$output "/bin/light")
                                (string-append #$output "/bin/qlight"))
                   (rename-file (string-append #$output "/bin/vis")
                                (string-append #$output "/bin/qvis"))))
               (add-after 'install-license-files 'clean-up-bin-directory
                 (lambda _
                   ;; Install target copies text documents to #$output/bin, move
                   ;; them to #$output/share/doc.
                   (delete-file (string-append #$output "/bin/gpl_v3.txt"))
                   (rename-file
                    (string-append #$output "/bin/changelog.txt")
                    (string-append #$output "/share/doc/"
                                   #$(package-name this-package) "-"
                                   #$(package-version this-package)
                                   "/changelog.txt"))
                   (rename-file
                    (string-append #$output "/bin/README.md")
                    (string-append #$output "/share/doc/"
                                   #$(package-name this-package) "-"
                                   #$(package-version this-package)
                                   "/README.md")))))
           #:tests? #f)) ; No tests
    (inputs (list embree-2))
    (home-page "https://ericwa.github.io/ericw-tools/")
    (synopsis "Map compiling tools for Quake/Hexen 2")
    (description
     "This package provides a collection of command line utilities used for
building Quake maps as well as working with various Quake file formats.  The
utilities include @command{qbsp} for building the geometry, @command{qvis} for
calculating visibility, @command{qlight} for lighting, @command{bspinfo} for
getting information, and @command{bsputil} for basic editing of data in a map
file.")
    (license license:gpl2+)))

(define-public eureka
  (package
    (name "eureka")
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/eureka-editor/Eureka/"
                                  version "/eureka-"
                                  ;; version without dots e.g 1.21 => 121
                                  (string-join (string-split version #\.) "")
                                  "-source.tar.gz"))
              (sha256
               (base32
                "1x4idjniz9sma3j9ss6ni7fafmz22zs2jnpsqw4my9rsnmra5d9v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'prepare-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share"))

               (with-fluids ((%default-port-encoding #f))
                 (substitute* "./src/main.cc"
                   (("/usr/local") out)))

               (substitute* "Makefile"
                 (("-o root") ""))))))))
    (inputs `(("mesa" ,mesa)
              ("libxft" ,libxft)
              ("libxinerama" ,libxinerama)
              ("libfontconfig" ,fontconfig)
              ("libjpeg" ,libjpeg-turbo)
              ("libpng" ,libpng)
              ("fltk" ,fltk-1.3)
              ("zlib" ,zlib)))
    (native-inputs (list pkg-config xdg-utils))
    (synopsis "Doom map editor")
    (description "Eureka is a map editor for the classic DOOM games, and a few
related games such as Heretic and Hexen.  It comes with a 3d preview mode and
a 2D editor view.")
    (home-page "https://eureka-editor.sourceforge.net/")
    (license license:gpl2+)))

(define-public guile-chickadee
  (package
    (name "guile-chickadee")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/chickadee/"
                                  "chickadee-" version ".tar.gz"))
              (sha256
               (base32
                "0x8g0bsvir2z3876ynslfgnmfr5p92ic4666v73526lswnv56bqk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
    (propagated-inputs
     `(("guile-opengl" ,guile3.0-opengl)
       ("guile-sdl2" ,guile-sdl2)))
    (inputs
     (list freetype
           guile-3.0-latest
           libjpeg-turbo
           libpng
           libvorbis
           mpg123
           openal
           readline))
    (native-inputs
     (list guile-3.0-latest pkg-config texinfo))
    (home-page "https://dthompson.us/projects/chickadee.html")
    (synopsis "Game development toolkit for Guile Scheme with SDL2 and OpenGL")
    (description "Chickadee is a game development toolkit for Guile Scheme
built on top of SDL2 and OpenGL.  Chickadee aims to provide all the features
that parenthetically inclined game developers need to make 2D (and eventually
3D) games in Scheme, such as:

@enumerate
@item extensible, fixed-timestep game loop
@item OpenGL-based rendering engine
@item keyboard, mouse, controller input
@item REPL-driven development model
@end enumerate\n")
    (license license:asl2.0)))

(define-public bennu-game-development
  (package
    (name "bennu-game-development")
    (version "353")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "http://svn.code.sf.net/p/bennugd/code")
                    (revision (string->number version))))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1iri58ryk9lbqn585cbccnvrfkj8qxlbcsk8rpih40jhvs1j101l"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "3rdparty") #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure-to-use-openssl
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "core")
             (delete-file "configure")
             (substitute* "configure.in"
               (("i\\*86\\)")
                "*)
                COMMON_CFLAGS=\"$COMMON_CFLAGS -DUSE_OPENSSL\"
                COMMON_LDFLAGS=\"$COMMON_LDFLAGS\"
                LIBSSL=\"crypto\"
                USE_OPENSSL=yes
                ;;

            i*86)"))
               #t)))))
    (inputs (list openssl zlib))
    (native-inputs (list pkg-config autoconf automake libtool))
    (synopsis "Programming language to create games")
    (description "Bennu Game Development, also known as bennudg, is a
programming language tailored at game development.  It is the successor of
Fenix.")
    (home-page "https://sourceforge.net/projects/bennugd/")
    (license license:zlib)))

(define-public bennu-game-development-modules
  (package
    (inherit bennu-game-development)
    (name "bennu-game-development-modules")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-conflicting-definitions
           (lambda _
             (with-fluids ((%default-port-encoding #f))
               (substitute* "core/include/fmath.h"
                 (("extern fixed fmul\\( int x, int y \\);") "")
                 (("extern fixed fdiv\\( int x, int y \\);") "")))
             (chdir "modules"))))))
    (inputs (list zlib libpng openssl sdl-mixer bennu-game-development))
    (synopsis "Modules for the Bennu Game Development programming language")
    (description "This package contains a collection of modules for the Bennu
Game Development programming language, from CD handling through SDL to
joystick support.")))

(define-public plib
  (package
    (name "plib")
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://plib.sourceforge.net/dist/"
                                  "plib-" version ".tar.gz"))
              (sha256
               (base32
                "0cha71mflpa10vh2l7ipyqk67dq2y0k5xbafwdks03fwdyzj4ns8"))
              (patches (search-patches "plib-CVE-2011-4620.patch"
                                       "plib-CVE-2012-4552.patch"))))
    (build-system gnu-build-system)
    ;; plib exists only as a static library, per the author's choice (see:
    ;; https://sourceforge.net/p/plib/mailman/message/10289018/).  Build it
    ;; with PIC, so that shared programs can at least "link" to it.
    (arguments (list #:configure-flags #~(list "CXXFLAGS=-fPIC")))
    (native-inputs (list autoconf automake pkg-config))
    (inputs (list mesa libxi libxmu))
    (home-page "https://plib.sourceforge.net/")
    (synopsis "Suite of portable game libraries")
    (description "PLIB is a set of libraries that will permit programmers to
write games and other realtime interactive applications that are 100% portable
across a wide range of hardware and operating systems.  PLIB includes sound
effects, music, a complete 3D engine, font rendering, a simple Windowing
library, a game scripting language, a GUI, networking, 3D math library and a
collection of handy utility functions.  All are 100% portable across nearly
all modern computing platforms.  Each library component is fairly independent
of the others")
    (license license:lgpl2.0+)))

(define-public ioquake3
  ;; We follow master since it seems that there won't be releases after 1.3.6.
  (let ((revision "2")
        (commit "29b0cc3a4d037046eb3247fc04f4b703f6a33452"))
    (package
      (name "ioquake3")
      (version (git-version "1.3.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ioquake/ioq3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fqq2qpnrgpgf3gs71wvxlkcihxcrvhvllh88ii4ip134c1qbs9q"))))
      (build-system gnu-build-system)
      (inputs
       (list curl
             freetype
             libjpeg-turbo
             libogg
             libvorbis
             openal
             opus
             opusfile
             sdl2))
      (native-inputs
       (list which                      ; else SDL_version.h won't be found.
             pkg-config))
      (arguments
       (list
        #:tests? #f                     ; no tests
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                "USE_INTERNAL_LIBS=0"
                "USE_FREETYPE=1"
                "USE_RENDERER_DLOPEN=0"
                "USE_OPENAL_DLOPEN=0"
                "USE_CURL_DLOPEN=0")
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ; no configure-script
            (replace 'install
              (lambda* (#:key make-flags outputs #:allow-other-keys)
                (apply invoke "make" "copyfiles"
                       (string-append "COPYDIR="
                                      (assoc-ref outputs "out")
                                      "/bin")
                       make-flags))))))
      (home-page "https://ioquake3.org/")
      (synopsis "FPS game engine based on Quake 3")
      (description "ioquake3 is a free software first person shooter engine
based on the Quake 3: Arena and Quake 3: Team Arena source code.  Compared to
the original, ioquake3 has been cleaned up, bugs have been fixed and features
added.  The permanent goal is to create a Quake 3 distribution upon which
people base their games, ports to new platforms, and other projects.")
      (license license:gpl2))))

(define-public inform
    (package
      (name "inform")
      (version "6.42")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://jxself.org/git/inform.git")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gb7b8y4qq9n3r4giqr4shzn3xli6aiaax7k4lzlgic7w1x3zjfl"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake))
      (synopsis "The Inform 6 compiler")
      (description
       "Inform 6 is a programming language designed for interactive fiction.
This version of the compiler has been modified slightly to work better when the
Inform standard library is in a non-standard location.")
      (home-page "https://jxself.org/git/inform.git")
      (license license:gpl3+)))

(define-public informlib
  (package
    (name "informlib")
    (version "6.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://jxself.org/git/informlib.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fcnw4jjzln402qk097n2s8y24vw1p3mmlmh6k1mbr2zfajjcn5r"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "lib"))))
    (synopsis "Inform 6 standard library")
    (description
     "This package provides the standard library for Inform 6.")
    (home-page "https://jxself.org/git/informlib.git")
    (license license:agpl3+)))

(define-public instead
  (package
    (name "instead")
    (version "3.3.5")
    (build-system cmake-build-system)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/instead-hub/instead")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02j8cw623j51qmr4991i5hsbrzmnp0qfzds8m6nwwr15sjv3hv1g"))
       (patches
        (search-patches
         "instead-use-games-path.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/zlib")))))
    (arguments
     '(#:configure-flags
       (list (string-append
              "-DLUA_INCLUDE_DIR="
              (assoc-ref %build-inputs "luajit") "/include/luajit-2.1/")
             "-DWITH_LUAJIT=1"
             "-DWITH_GTK3=1")
       #:tests? #f))
    (inputs
     `(("gtk+",gtk+)
       ("lua" ,lua)
       ("luajit" ,luajit)
       ("pkg-config" ,pkg-config)
       ("sdl2-images" ,sdl2-image)
       ("sdl2-ttf" ,sdl2-ttf)
       ("sdl2-mixer" ,sdl2-mixer)
       ("zlib" ,zlib)))
    (home-page "https://instead3.syscall.ru/")
    (synopsis "Text adventure interpreter")
    (description "The STEAD (Simple TExt ADventures) interpreter provides
functionality to play games that mix elements of visual novels, interactive
fiction and classic point-and-click adventures.")
    (native-search-paths
     (list (search-path-specification
            (variable "INSTEAD_GAMES_PATH")
            (separator #f)                        ;single entry
            (files '("share/instead/games")))))
    (license license:expat)))

(define-public openvr
  (package
    (name "openvr")
    (version "1.26.7")
    (home-page "https://github.com/ValveSoftware/openvr/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09rvrja3pz6ggs41ra71p4dwjl4n0rpqrqw8jiy92xl33hhxbsmx"))))
    (build-system cmake-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED=1")))
    (synopsis "Virtual reality software development kit")
    (description "OpenVR is an API and runtime that allows access to VR
hardware from multiple vendors without requiring that applications have
specific knowledge of the hardware they are targeting.")
    (license license:bsd-3)))

(define-public flatzebra
  (package
    (name "flatzebra")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://perso.b2b2c.ca/~sarrazip/dev/"
                           "flatzebra-" version ".tar.gz"))
       (sha256
        (base32 "1p1igi757m9a46v29mm7r40x61kdj7j66b9dbn53l5yfhnwa4w93"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list sdl2 sdl2-gfx sdl2-image sdl2-mixer sdl2-ttf))
    (home-page "http://perso.b2b2c.ca/~sarrazip/dev/burgerspace.html")
    (synopsis "Generic game engine for 2D double-buffering animation")
    (description
     "Flatzebra is a simple, generic C++ game engine library supporting 2D
double-buffering.")
    (license license:gpl2+)))

(define-public freesolid
  (package
    (name "freesolid")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/FreeSOLID-"
                                  version ".zip"))
              (sha256
               (base32
                "0wxqiv1ba227kwxpgwf6in9ai1lcamhmp1ib1c1chq4xvnpwdvc9"))
              (patches (search-patches "freesolid-autotools.patch"
                                       "freesolid-pkgconfig.patch"
                                       "freesolid-configure.patch"
                                       "freesolid-automake.patch"))))
    (build-system gnu-build-system)
    (arguments (list #:phases #~(modify-phases %standard-phases
                                  (add-after 'unpack 'force-reboostrap
                                    (lambda _
                                      (delete-file "bootstrap.sh")
                                      (delete-file "configure"))))))
    (native-inputs (list autoconf automake libtool unzip))
    (home-page "https://sourceforge.net/projects/freesolid/")
    (synopsis "3D collision detection C++ library")
    (description "FreeSOLID is a library for collision detection of
three-dimensional objects undergoing rigid motion and deformation.  FreeSOLID
is designed to be used in interactive 3D graphics applications.")
    (license license:lgpl2.0+)))

(define-public libccd
  (package
    (name "libccd")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danfis/libccd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sfmn5pd7k5kyhbxnd689xmsa5v843r7sska96dlysqpljd691jc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_DOCUMENTATION=ON"
                           "-DBUILD_TESTING=ON"
                           "-DENABLE_DOUBLE_PRECISION=ON")))
    (native-inputs
     (list python-sphinx))
    (home-page "https://github.com/danfis/libccd")
    (synopsis "Library for collision detection between two convex shapes")
    (description "@code{libccd} is library for a collision detection
between two convex shapes.  @code{libccd} implements variation on
Gilbert–Johnson–Keerthi algorithm plus Expand Polytope Algorithm (EPA)
and also implements algorithm Minkowski Portal Refinement (MPR,
a.k.a. XenoCollide) as described in Game Programming Gems 7.")
    (license license:expat)))

(define-public ode
  (package
    (name "ode")
    (version "0.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/odedevs/ode/downloads/"
                           "ode-" version ".tar.gz"))
       (sha256
        (base32 "0ya6slmy2iysx3fql7w7r56c7gsk93qp1apfjn3raw252vfmx1xs"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libccd")))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; XXX: The sole test is failing on i686 due to a rounding error.
      #:tests? (not (or (target-x86-32?)
                        (%current-target-system)))
      #:configure-flags #~(list "-DODE_WITH_LIBCCD_SYSTEM=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-libccd
            (lambda _
              (substitute* "CMakeLists.txt"
                (("configure_file\\(libccd/.*") "")))))))
    (inputs
     (list glu libccd mesa))
    (home-page "https://www.ode.org/")
    (synopsis "High performance library for simulating rigid body dynamics")
    (description "ODE is a high performance library for simulating
rigid body dynamics.  It is fully featured, stable, mature and
platform independent with an easy to use C/C++ API.  It has advanced
joint types and integrated collision detection with friction.  ODE is
useful for simulating vehicles, objects in virtual reality
environments and virtual creatures.  It is currently used in many
computer games, 3D authoring tools and simulation tools.")
    ;; Software is dual-licensed.
    (license (list license:lgpl2.1+ license:expat))))

(define-public chipmunk
  (let ((commit "d0239ef4599b3688a5a336373f7d0a68426414ba")
        (revision "1"))
    (package
      (name "chipmunk")
      (version (git-version "7.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slembcke/Chipmunk2D")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1910rfnanhna99bhfiyny3ki7aip2i9p4jzmwsfcg16m9gip5fd6"))
         (modules '((guix build utils)))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                ;no test
             #:configure-flags
             #~(list "-DBUILD_STATIC=OFF"
                     "-DBUILD_DEMOS=OFF")))
      (inputs
       (list freeglut libxmu libxrandr))
      (home-page "https://chipmunk-physics.net/")
      (synopsis "Fast and lightweight 2D game physics library")
      (description "Chipmunk is a simple, lightweight, fast and portable 2D
rigid body physics library written in C.")
      (license license:expat))))

(define-public box2d
  (package
    (name "box2d")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erincatto/box2d")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ja9cahf3z9zzrdaqcw44lpjmqf2ir2g4chwz0iwqwlkckwhpgvh"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled code only used for the testbed.
           (delete-file-recursively "extern")
           ;; Remove bundled copy of doctest, and adjust tests accordingly.
           (delete-file "unit-test/doctest.h")
           (substitute* "unit-test/CMakeLists.txt"
             (("doctest\\.h")
              ""))
           (substitute* (find-files "unit-test" "\\.cpp$")
             (("include \"doctest\\.h\"")
              "include <doctest/doctest.h>"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "unit_test"
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DBOX2D_BUILD_TESTBED=OFF")))
    (native-inputs
     (list doctest))                    ;for tests
    (inputs
     (list libx11))
    (home-page "https://box2d.org/")
    (synopsis "2D physics engine for games")
    (description "Box2D is a 2D rigid body simulation library for games.
Programmers can use it in their games to make objects move in realistic ways and
make the game world more interactive.  From the game engine's point of view, a
physics engine is just a system for procedural animation.")
    (license license:expat)))

(define-public box2d-3
  (package
   (inherit box2d)
   (name "box2d")
   (version "3.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/erincatto/box2d")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0m01c23mxvg96zypqyi2fpkd1dsvgflafi3ncga6ihdvxbwaybk5"))))
   (build-system cmake-build-system)
   (arguments
    (substitute-keyword-arguments
        (package-arguments box2d)
      ((#:test-target _) "")            ; no check
      ((#:configure-flags original-flags)
       `(cons* "-DBOX2D_UNIT_TESTS=OFF" ; enkiTS need for all test apps
               "-DBOX2D_SAMPLES=OFF"
               (delete "-DBOX2D_BUILD_TESTBED=OFF" ,original-flags)))))))

(define-public libtcod
  (package
    (name "libtcod")
    (version "1.15.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libtcod/libtcod")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pzr8ajmbqvh43ldjajx962xirj3rf8ayh344p6mqlrmb8gxrfr5"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (delete-file-recursively "src/vendor/utf8proc")
                          (delete-file-recursively "src/vendor/zlib")
                          (delete-file "src/vendor/stb_truetype.h")
                          (delete-file "src/vendor/stb_sprintf.h")
                          (delete-file "src/vendor/lodepng.cpp")
                          (delete-file "src/vendor/lodepng.h")

                          (substitute* "buildsys/autotools/sources.am"
                            (("\\.\\./\\.\\./src/vendor/lodepng\\.cpp \\\\\n") "")
                            (("\\.\\./\\.\\./src/vendor/stb\\.c \\\\")
                             "../../src/vendor/stb.c")
                            (("\\.\\./\\.\\./src/vendor/utf8proc/utf8proc\\.c") ""))

                          (substitute* "src/libtcod/sys_sdl_img_png.cpp"
                            (("\\.\\./vendor/") ""))

                          (substitute* '("src/libtcod/color/canvas.cpp"
                                         "src/libtcod/sys_sdl_img_png.cpp"
                                         "src/libtcod/tileset/truetype.cpp"
                                         "src/libtcod/tileset/tilesheet.cpp")
                            (("\\.\\./\\.\\./vendor/") ""))

                          (substitute* "src/libtcod/console/printing.cpp"
                            (("\\.\\./\\.\\./vendor/utf8proc/") ""))
                          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gnu-ld"
                           "LIBS=-lutf8proc -llodepng")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _
             (chdir "buildsys/autotools")
             (patch-shebang "get_version.py"))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           python
           pkg-config
           stb-sprintf
           stb-truetype))
    (inputs
     (list lodepng sdl2 utf8proc zlib))
    (home-page "https://github.com/libtcod/libtcod")
    (synopsis "Library specifically designed for writing roguelikes")
    (description
     "libtcod is a fast, portable and uncomplicated API for roguelike
developers providing an advanced true color console, input, and lots of other
utilities frequently used in roguelikes.")
    (license license:bsd-3)))

(define-public warsow-qfusion
  ;; As of 2020-04-09, the latest stable version 2.1.0 is deprecated.
  ;; The 2.5 beta as published on the homepage is commit
  ;; c4de15df559410aff0ca6643724e24cddb0ecbbd
  (let ((commit "c4de15df559410aff0ca6643724e24cddb0ecbbd"))
    (package
      (name "warsow-qfusion")
      (version (git-version "2.5" "1" commit)) ; 2.5-beta
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Warsow/qfusion/")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xv2yycr43p3xmq7lm6j6zb3cpcr6w00x7qg918faq0mw9j7v48g"))
                ;; Issue reported here: https://github.com/Warsow/qfusion/issues/46
                (patches (search-patches "warsow-qfusion-fix-bool-return-type.patch"))
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "platforms")
                            (delete-file-recursively "debian")
                            (delete-file-recursively "libsrcs")
                            #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No tests.
         #:configure-flags '("-DQFUSION_GAME=Warsow")
         #:modules
         ((guix build utils)
          (guix build cmake-build-system)
          (ice-9 match))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'change-to-build-dir
             (lambda _
               (chdir "source")
               #t))
           (add-after 'install 'really-install
             (lambda* (#:key outputs system #:allow-other-keys)
               (let ((arch (match system
                             ("x86_64-linux" "x86_64")
                             ("i686-linux" "i386")))
                     (out (assoc-ref outputs "out")))
                 (install-file (string-append "../source/build/basewsw/libgame_"
                                              arch ".so")
                               (string-append out "/lib/"))
                 (install-file (string-append "../source/build/libui_" arch ".so")
                               (string-append out "/lib/"))
                 (for-each
                  (lambda (file)
                    (install-file file (string-append out "/bin/")))
                  (append (find-files "../source/build" "warsow")
                          (find-files "../source/build" "wsw_server."))))
               #t)))))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("curl" ,curl)
         ("freetype" ,freetype)
         ("ffmpeg" ,ffmpeg-4)
         ("libjpeg" ,libjpeg-turbo)
         ("libogg" ,libogg)
         ("libpng" ,libpng)
         ("libtheora" ,libtheora)
         ("libvorbis" ,libvorbis)
         ("mesa" ,mesa)
         ("openal" ,openal)
         ("pulseaudio" ,pulseaudio)
         ("qtbase" ,qtbase-5)
         ("qtdeclarative-5" ,qtdeclarative-5)
         ("sdl2" ,sdl2)
         ("uuid.h" ,util-linux "lib")
         ("zlib" ,zlib)))
      (native-inputs
       (list pkg-config))
      (home-page "https://github.com/Warsow/qfusion")
      (supported-systems '("i686-linux" "x86_64-linux"))
      (synopsis "Warsow's fork of qfusion, the id Tech 2 derived game engine")
      (description
       "This package contains the game engine of Warsow, a first-person
shooter video game.  The engine is based on qfusion, the id Tech 2 derived
game engine.  id Tech 2 is the engine originally behind Quake 2.")
      (license license:gpl2+))))

(define-public dhewm3
  (package
    (name "dhewm3")
    (version "1.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/dhewm/dhewm3")
                                  (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16cvf78a7q00bkf74waj6gss09y4iqn3zl9srsfg6i7336gjm2wn"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ; No tests.
           #:configure-flags
           ;; Needed to fix 32bit builds.
           #~(if (not #$(target-64bit?))
                 (list "-DCMAKE_CXX_FLAGS=-D_FILE_OFFSET_BITS=64")
                 '())
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'change-to-build-dir
                 (lambda _
                   (chdir "neo"))))))
    (inputs
     (list curl
           libx11
           openal
           sdl2
           zlib))
    (home-page "https://dhewm3.org/")
    (synopsis "Port of the original Doom 3 engine")
    (description
     "@command{dhewm3} is a source port of the original Doom 3 engine (not
Doom 3: BFG Edition), also known as id Tech 4.  Compared to the original
version of the Doom 3 engine, dhewm3 has many bugfixes, supports EAX-like
sound effects on all operating systems and hardware (via OpenAL Softs EFX
support), has much better support for widescreen resolutions and has 64bit
support.")
    (license license:gpl3)))

(define-public tesseract-engine
  (let ((svn-revision 2411))
    (package
      (name "tesseract-engine")
      (version (string-append "20200615-" (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.tuxfamily.org/svnroot/tesseract/main")
               (revision svn-revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1av9jhl2ivbl7wfszyhyna84llvh1z2d8khkmadm8d105addj10q"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("bin" "bin64"
                         ;; Remove "media" since some files such as
                         ;; media/sound/game/soundsnap/info.txt refer to a
                         ;; non-commercial license.
                         "media"
                         "server.bat"
                         "tesseract.bat"
                         "src/lib"
                         "src/lib64"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CC=gcc")
         #:tests? #f                    ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'cd-src
             (lambda _ (chdir "src") #t))
           (add-before 'build 'fix-env
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (search-input-directory inputs "include/SDL2")
                        ":" (or (getenv "CPATH") "")))))
           (add-after 'install 'really-install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share/tesseract"))
                      (bin (string-append out "/bin/tesseract"))
                      (client (string-append out "/bin/tesseract-client")))
                 (chdir "..")           ; Back to root.
                 (for-each
                  (lambda (dir)
                    (mkdir-p (string-append share "/" dir))
                    (copy-recursively dir (string-append share "/" dir)))
                  '("config"))
                 (mkdir-p (string-append out "/bin/"))
                 (copy-file "bin_unix/native_client" client)
                 (copy-file "bin_unix/native_server"
                            (string-append out "/bin/tesseract-server"))
                 (call-with-output-file bin
                   (lambda (p)
                     (format p "#!~a
TESS_DATA=~a
TESS_BIN=~a
TESS_OPTIONS=\"-u$HOME/.tesseract\"
cd \"$TESS_DATA\"
exec \"$TESS_BIN\" \"$TESS_OPTIONS\" \"$@\""
                             (which "bash")
                             share
                             client)))
                 (chmod bin #o755)
                 (install-file "src/readme_tesseract.txt"
                               (string-append out "/share/licenses/tesseract/LICENSE")))
               #t)))))
      (inputs
       `(("sdl2-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-image)))
         ("zlib" ,zlib)
         ("libpng" ,libpng)
         ("libgl" ,mesa)))
      (home-page "http://tesseract.gg/")
      (synopsis "First-person shooter engine with map editing, instagib, DM and CTF")
      (description "This package contains the game engine of Tesseract, a
first-person shooter focused on cooperative in-game map editing.

The engine is derived from @emph{Cube 2: Sauerbraten} technology but with
upgraded modern rendering techniques.  The new rendering features include
fully dynamic omnidirectional shadows, global illumination, HDR lighting,
deferred shading, morphological / temporal / multisample anti-aliasing, and
much more.")
      (license license:zlib))))

(define-public recastnavigation
  (package
    (name "recastnavigation")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/recastnavigation/recastnavigation")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rdz3qmp4b961zjah2ax82h471j14w2rcf576gcyx7vldrg8dmj8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               "-DRECASTNAVIGATION_DEMO=OFF"
                               "-DRECASTNAVIGATION_TESTS=ON"
                               "-DRECASTNAVIGATION_EXAMPLES=OFF")))
    (synopsis "Navigation system for games")
    (description "Recast is state of the art navigation mesh
construction toolset for games.

@itemize
@item It is automatic, which means that you can throw any level geometry
      at it and you will get robust mesh out.
@item It is fast which means swift turnaround times for level designers.
@end itemize

The Recast process starts with constructing a voxel mold from a level
geometry and then casting a navigation mesh over it.  The process
consists of three steps, building the voxel mold, partitioning the mold
into simple regions, peeling off the regions as simple polygons.

Recast is accompanied with Detour, path-finding and spatial reasoning
toolkit.  You can use any navigation mesh with Detour, but of course the
data generated with Recast fits perfectly.

Detour offers simple static navigation mesh which is suitable for many
simple cases, as well as tiled navigation mesh which allows you to plug
in and out pieces of the mesh.  The tiled mesh allows you to create
systems where you stream new navigation data in and out as the player
progresses the level, or you may regenerate tiles as the world changes.")
    (home-page "https://github.com/recastnavigation/recastnavigation")
    (license license:zlib)))

(define-public raylib
  (package
    (name "raylib")
    (version "5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/raysan5/raylib/")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; TODO: Unbundle src/external
              (sha256
               (base32
                "1dhy9ghbwvz0s434j03rfa2l6wxcfj028vlkk1xbf5q97vin5pr7"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f  ;no test
           #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=ON"
                   "-DUSE_EXTERNAL_GLFW=ON"
                   "-DCMAKE_C_FLAGS=-lpulse")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'configure-miniaudio
                 ;; Use PulseAudio as raudio backend.
                 (lambda _
                   (substitute* "src/raudio.c"
                     (("^#include \"external/miniaudio\\.h\"") "
#define MA_NO_RUNTIME_LINKING
#define MA_ENABLE_ONLY_SPECIFIC_BACKENDS
#define MA_ENABLE_PULSEAUDIO
#include \"external/miniaudio.h\"
"))))
               (add-after 'install 'install-api-files
                 ;; For generating bindings.
                 (lambda _
                   (copy-recursively
                    (string-append #$source "/parser/output")
                    (string-append #$output "/share/raylib")))))))
    (inputs (list glfw-3.4 pulseaudio))
    (native-inputs (list pkg-config))
    (synopsis "C library for videogame programming")
    (description
     "raylib is a high-level library for video game programming.  It aims to
  abstract away platform and graphics details, allowing you to focus on
  writing your game.")
    (home-page "https://www.raylib.com/")
    (license license:zlib)))

(define-public tic80
  ;; Use an unreleased version for 'PREFER_SYSTEM_LIBRARIES'.
  (let ((commit "fcfd7c9862e9157512bcab53affecd592b320131")
        ;; These C libraries are used in source form by tic80.
        (3rd/jsmn
         (origin                        ;Expat
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/zserge/jsmn")
                 (commit "25647e692c7906b96ffd2b05ca54c097948e879c")))
           (file-name "jsmn-checkout")
           (sha256
            (base32
             "19xgrap95a8ziicgd0c3fns51z1g4q06b5lb5pg76ah4ychhgg5p"))))
        (3rd/blip-buf
         (origin                        ;LGPL2.1+
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/nesbox/blip-buf")
                 (commit "330226d9b55ecbeea644e17b5e0f096a165ca07e")))
           (file-name "blip-buf-checkout")
           (sha256
            (base32
             "0fycffd6pbh9ilmr032dlrwd6dhvpkjp2r9x98r0kmwqpxc4x90d"))))
        (3rd/msf-gif
         (origin                        ;Expat or Public Domain
           (method url-fetch)
           (uri (string-append
                 "https://github.com/notnullnotvoid/msf_gif/releases/download/"
                 "v2.3/msf_gif.h"))
           (sha256
            (base32
             "1ivjwwqxqjfhm8caz1srkp8wx7fpzvpf7s26ifif7cryvqch8vnf")))))
    (package
      (name "tic80")
      (version (git-version "1.2.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nesbox/TIC-80")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17zxfiji6cb9ad7j3l82bdig0k7bz77bzwg7m0vd9ywrwk0kgxjk"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (delete-file-recursively "vendor")
              (copy-recursively #$3rd/jsmn "vendor/jsmn")
              (copy-recursively #$3rd/blip-buf "vendor/blip-buf")
              (mkdir "vendor/msf_gif")
              (copy-file #$3rd/msf-gif "vendor/msf_gif/msf_gif.h")))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                ;no tests
             #:configure-flags
             #~'("-DBUILD_STATIC=ON" ;don't build runtimes as shared libraries
                 "-DPREFER_SYSTEM_LIBRARIES=ON"
                 "-DCMAKE_EXE_LINKER_FLAGS=-lpulse" ;for miniaudio
                 ;; TODO: moon, python, wren
                 "-DBUILD_WITH_FENNEL=ON"
                 "-DBUILD_WITH_JANET=ON"
                 "-DBUILD_WITH_JS=ON"
                 "-DBUILD_WITH_LUA=ON"
                 "-DBUILD_WITH_RUBY=ON"
                 "-DBUILD_WITH_SCHEME=ON"
                 "-DBUILD_WITH_SQUIRREL=ON"
                 "-DBUILD_WITH_WASM=ON")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'prepare-sources
                   (lambda _
                     (let* ((s7 #$(this-package-input "s7"))
                            (fennel #$(this-package-input "fennel")))
                       (install-file (string-append s7 "/include/s7.h")
                                     "vendor/s7/")
                       (install-file (string-append s7 "/share/s7/s7.c")
                                     "vendor/s7/")
                       (mkdir "vendor/fennel")
                       (copy-file (car (find-files fennel "fennel\\.lua"))
                                  "vendor/fennel/loadfennel.lua")
                       (with-directory-excursion "vendor/fennel"
                         (substitute* "loadfennel.lua"
                           (("return mod") "package.loaded['fennel'] = mod"))
                         (invoke "xxd" "-i" "loadfennel.lua" "fennel.h")))
                     (substitute* "src/api/mruby.c"
                       (("#include <mruby\\.h>" all)
                        (string-append
                         all "\n#include <mruby/internal.h>")))
                     (substitute* "src/ext/fft.c"
                       (("#include \"miniaudio\\.h\"") "
#define MA_NO_RUNTIME_LINKING
#define MA_ENABLE_ONLY_SPECIFIC_BACKENDS
#define MA_ENABLE_PULSEAUDIO
#define MA_ENABLE_NULL
#include \"miniaudio.h\"
")))))))
      (native-inputs
       (list pkg-config xxd))
      (inputs
       (list argparse
             fennel
             giflib
             janet
             kubazip
             libpng
             lua
             mruby
             naett
             pulseaudio
             quickjs
             s7
             sdl2
             squirrel
             wasm3))
      (synopsis "Fantasy tiny computer")
      (home-page "https://tic80.com/")
      (description
       "TIC-80 is a fantasy computer for making, playing and sharing tiny
games.  There are built-in tools for development: code, sprites, maps, sound
editors and the command line, which is enough to create a mini retro game.")
      (license license:expat))))

(define-public bbcsdl
  (package
    (name "bbcsdl")
    (version "1.39a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rtrussell/BBCSDL/")
                    (commit "93b0ffae960f4c4f45fdc2202bc6e83ee5ca277c")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03ga14k2hbhflnaynbyx9lwlbxlzx3rv6zqq21yhl183s6d4c0wa"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; XXX: tests not automated
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (replace 'build
            (lambda* (#:key outputs #:allow-other-keys)
              ;; 'makefile' expects the source directory to be named 'BBCSDL'.
              (symlink "source" "../BBCSDL")
              ;; 'bbcsdl' finds 'libstb.so' in its RPATH.
              (substitute* "bin/linux/makefile"
                (("-Wl,-R,'\\$\\$ORIGIN'")
                 (string-append "-Wl,-rpath="
                                (assoc-ref outputs "out") "/opt/bbcsdl")))
              ;; Build 'bbcbasic' and 'bbcsdl'.
              (invoke "make" "-C" "console/linux")
              (invoke "make" "-C" "bin/linux")))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (opt (string-append out "/opt/bbcsdl"))
                     (bin (string-append out "/bin")))
                (for-each
                 (lambda (f)
                   (copy-recursively f (string-append opt "/" f)))
                 ;; Those files need to be installed into the same difertory.
                 '("lib" "examples" "bbcsdl.bbc"
                   "libstb.so" "bbcsdl" "bbcbasic"))
                ;; Replace bundled fonts.
                (for-each
                 (lambda (font)
                   (delete-file (string-append opt "/lib/" font))
                   (symlink
                    (search-input-file
                     inputs (string-append "share/fonts/truetype/" font))
                    (string-append opt "/lib/" font)))
                 '("DejaVuSans.ttf" "DejaVuSansMono.ttf"
                   "DejaVuSans-Oblique.ttf"
                   "FreeSans.ttf" "FreeMono.ttf" "FreeSerif.ttf"))
                (mkdir bin)
                (symlink (string-append opt "/bbcsdl")
                         (string-append bin "/bbcsdl"))
                (symlink (string-append opt "/bbcbasic")
                         (string-append bin "/bbcbasic"))))))))
    (native-inputs (list nasm))
    (inputs (list sdl2 sdl2-ttf sdl2-net font-dejavu font-gnu-freefont))
    (synopsis "BBC BASIC for SDL 2.0")
    (home-page "https://www.bbcbasic.co.uk/bbcsdl/")
    (description
     "BBC BASIC is the programming language originally specified and adopted
by the British Broadcasting Corporation for its groundbreaking Computer
Literacy Project of the early 1980s.  BBC BASIC for SDL 2.0 combines the
simplicity of BASIC with the sophistication of a modern structured language,
allowing you to write utilities and games, use sound and graphics, perform
calculations and create complete applications.")
    (license license:zlib)))
