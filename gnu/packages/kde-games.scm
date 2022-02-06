;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu packages kde-games)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module ((gnu packages gnome) #:select (shared-mime-info))
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public ktuberling
  (package
    (name "ktuberling")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ktuberling-" version ".tar.xz"))
      (sha256
       (base32 "1awsn285j9nggyypkra9ladgi46w2m7m09d8364w5d0sygpzmgsg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools perl))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           python-wrapper
           qtbase-5
           qtmultimedia-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/ktuberling/")
    (synopsis "Stamp drawing toy")
    (description "KTuberling is a drawing toy intended for small children and
adults who remain young at heart.  The game has no winner; the only purpose is
to make the funniest faces you can.  Several activities are possible, e.g.:

@itemize
@item Give the potato a funny face, clothes, and other goodies
@item Build a small town, complete with school, zoo, and fire department
@item Create a fantastic moonscape with spaceships and aliens
@item Top a pizza
@end itemize

KTuberling can speak the name of each the object in several languages,
to assist in learning basic vocabulary.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public picmi
  (package
    (name "picmi")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/picmi-" version ".tar.xz"))
      (sha256
       (base32 "0gk1yq5ac55k6lxbxszxpd393fb9k6yphisb71lx2zv9gchl44n6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           kio
           knewstuff
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/picmi/")
    (synopsis "Number logic game")
    (description "Picmi is a number logic game in which cells in a grid have
to be colored or left blank according to numbers given at the side of the
grid.  The aim is to reveal a hidden picture.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kolf
  (package
    (name "kolf")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolf-" version ".tar.xz"))
       (sha256
        (base32 "00dhjy82d9964z94nn4vkkwynql3bfa6djwrgsq93f9d7grgkd7g"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           ktextwidgets
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kolf/")
    (synopsis "Miniature golf game")
    (description "Kolf is a miniature golf game for one to ten players.  The
game is played from an overhead view, with a short bar representing the golf
club.  Kolf features many different types of objects, such as water hazards,
slopes, sand traps, and black holes (warps), among others.

Features are:
@itemize
@item Single and Multi-player (up to ten players) modes
@item High scores table
@item Dynamic courses
@item Third-party courses
@item Course editor
@end itemize

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public libkmahjongg
  (package
    (name "libkmahjongg")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/libkmahjongg-" version ".tar.xz"))
      (sha256
       (base32 "114viyqq7zlwsdnm96iyyvj8ma4p06m69hs641yv42xlbkspwbal"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kauth
           kcompletion
           ;("kconfig" ,kconfig)
           kcodecs
           kconfigwidgets
           kcoreaddons
           ki18n
           kwidgetsaddons
           qtbase-5
           qtsvg-5))
    (home-page "https://games.kde.org/")
    (synopsis "Shared library for kmahjongg and kshisen")
    (description "Shared library and common files for kmahjongg, kshisen and
other Mah Jongg like games.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmahjongg
  (package
    (name "kmahjongg")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kmahjongg-" version ".tar.xz"))
      (sha256
       (base32 "02yvvpwkk5gbj445zv5xhfragk8220rlx0pkxf32pj0jsv7dnz1x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           knewstuff
           kxmlgui
           libkdegames
           libkmahjongg
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kmahjongg/")
    (synopsis "Tile laying patience")
    (description "In KMahjongg the tiles are scrambled and staked on top of
each other to resemble a certain shape.  The player is then expected to remove
all the tiles off the game board by locating each tile's matching pair.

A variety of tile layouts are included, as well as an editor to create new
layouts.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kshisen
  (package
    (name "kshisen")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kshisen-" version ".tar.xz"))
      (sha256
       (base32 "1i11gh87gfza58rpdd44pjb423an9a44cls117ba9gznxm67cph5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           ;("perl" ,perl)
           ;("pkg-config" ,pkg-config)
           kdoctools))
    (inputs
     (list kauth
           kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           libkmahjongg
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kshisen/")
    (synopsis "Shisen-Sho solitaire game")
    (description "KShisen is a solitaire-like game played using the standard
set of Mahjong tiles.  Unlike Mahjong however, KShisen has only one layer of
scrambled tiles

This package is part of the KDE games module.")
    (license license:gpl2+)))

(define-public kajongg
  (package
    (name "kajongg")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kajongg-" version ".tar.xz"))
      (sha256
       (base32 "1sffssfpzsd83ippkwpmqdx8rfh9cpd7i22nsv8asnaylylvy3zd"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/mjresource.py"
               (("'share', 'kmahjongglib'" all)
                (string-append "'" (assoc-ref inputs "libkmahjongg")
                               "/share', 'kmahjongglib'")))))
         (add-after 'qt-wrap 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (program)
                           (wrap-program program
                             `("GUIX_PYTHONPATH" ":" prefix
                               (,(getenv "GUIX_PYTHONPATH")))))
                         (list (string-append out "/bin/kajongg")
                               (string-append out "/bin/kajonggserver")))))))))
    (native-inputs
     (list extra-cmake-modules
           ;("perl" ,perl)
           kdoctools))
    (inputs
     (list bash-minimal
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           libkmahjongg
           python
           python-twisted
           python-pyqt
           python-zope-interface
           qtbase-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kajongg/")
    (synopsis "Classical Mah Jongg game for 4 players")
    (description "Kajongg is the ancient Chinese board game for 4 players.

If you are looking for the Mah Jongg solitaire please use the application
kmahjongg.

Kajongg can be used in two different ways: Scoring a manual game where you
play as always and use Kajongg for the computation of scores and for
bookkeeping.  Or you can use Kajongg to play against any combination of other
human players or computer players.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kbreakout
  (package
    (name "kbreakout")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kbreakout-" version ".tar.xz"))
      (sha256
       (base32 "0vqlxaggzvvrb439ybsvd5kr9j2jzpwk4xy3yni83y830h1mmhhc"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kbreakout/")
    (synopsis "Breakout like game")
    (description "KBreakout is similar to the classics breakout and xboing,
featuring a number of added graphical enhancements and effects.  You control a
paddle at the bottom of the playing-field, and must destroy bricks at the top
by bouncing balls against them.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kmines
  (package
    (name "kmines")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kmines-" version ".tar.xz"))
      (sha256
       (base32 "1wxy0cyz733wvnxfjhirqf41wnda4f6aqdiqmb5r1ngzzllgbglc"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kmines/")
    (synopsis "Classical mine sweeper game")
    (description "KMines is a classic Minesweeper game.  The idea is to
uncover all the squares without blowing up any mines.  When a mine is blown
up, the game is over.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public konquest
  (package
    (name "konquest")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/konquest-" version ".tar.xz"))
      (sha256
       (base32 "0lrahq9s70rx24dw4cgpvchr4s6pcl565vh343ggg24s1rd3ly80"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/konquest/")
    (synopsis "Simple turn-based strategy game")
    (description "Konquest is the KDE version of Gnu-Lactic Konquest.  Players
conquer other planets by sending ships to them.  The goal is to build an
interstellar empire and ultimately conquer all other player's planets.  The
game can be played with up to nine empires, commanded either by the computer
or by puny earthlings.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kbounce
  (package
    (name "kbounce")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kbounce-" version ".tar.xz"))
      (sha256
       (base32 "1am4j11cjzlmav2zh5802kasy0kdcx78slycadnf96bmhxs8hvyv"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kbounce/")
    (synopsis "Jezzball arcade game")
    (description "KBounce is a single player arcade game with the elements of
puzzle.  It is played on a field, surrounded by wall, with two or more balls
bouncing around within the walls.  The object of the game is to build new
walls to decrease the size of the active field.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kblocks
  (package
    (name "kblocks")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kblocks-" version ".tar.xz"))
      (sha256
       (base32 "1n3jc96ws8078gk1il61dc96p3pzvj3z9brnwi274pk4cif63bli"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kblocks/")
    (synopsis "Single player falling blocks puzzle game")
    (description "KBlocks is the classic Tetris-like falling blocks game.

The idea is to stack the falling blocks to create horizontal lines without any
gaps.  When a line is completed it is removed, and more space is available in
the play area.  When there is not enough space for blocks to fall, the game is
over.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksudoku
  (package
    (name "ksudoku")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/ksudoku-" version ".tar.xz"))
      (sha256
       (base32 "1gw0ybwhvg1z8pcs72f73y52jvzvrw367g275axf2rw50iik6jwv"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kguiaddons
           ki18n
           kiconthemes
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           glu
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/ksudoku/")
    (synopsis "Sudoku puzzle game and solver")
    (description "KSudoku is a Sudoku game and solver, supporting a range of
2D and 3D Sudoku variants.  In addition to playing Sudoku, it can print Sudoku
puzzle sheets and find the solution to any Sudoku puzzle.

The word Sudoku means \"single number in an allotted place\" in Japanese.
These are the basic rules: Every Sudoku is a square divided into 3x3
subsquares with 3x3 cells each.

Some cells are filled with a number at the beginning.  The remaining ones are
to be filled by the player using numbers from 1 to 9, without repeating a
number twice on each column, row or subsquare (each of them must contain only
one 1, one 2, one 3, and so on).  The game requires logic and patience.
Solving takes usually 10 to 30 minutes, depending on puzzle level, your skill
and experience.

The numerals in Sudoku puzzles are used for convenience (for example in 16x16
board we use letters): arithmetic relationships between numbers are
irrelevant.

This program supports also 16x16 games with numbers from 1 to 16 and 256
cells with 16 cols, rows and subsquares!

More information at http://en.wikipedia.org/wiki/Sudoku

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public klines
  (package
    (name "klines")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/klines-" version ".tar.xz"))
      (sha256
       (base32 "1ypi64wdsw1zsj03wcxj02v27y1by113v89as8dyk9wr0pfmbpqf"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/klines/")
    (synopsis "Place 5 equal pieces together, but wait, there are 3 new ones")
    (description "KLines is a simple but highly addictive one player game.

The player has to move the colored balls around the game board, gathering them
into the lines of the same color by five.  Once the line is complete it is
removed from the board, therefore freeing precious space.  In the same time
the new balls keep arriving by three after each move, filling up the game
board.

KLines is a single-player game where the player removes colored balls from the
board by arranging them into lines of five or more.  However, every time the
player moves a ball, three more balls are added to the board.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kgoldrunner
  (package
    (name "kgoldrunner")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kgoldrunner-" version ".tar.xz"))
      (sha256
       (base32 "0gzz58407zjmk311kyyj5l2c1ciczcq9i8ckpwbd341dvwaww27q"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kgoldrunner/")
    (synopsis "Action and puzzle solving game")
    (description "KGoldrunner is an action game where the hero runs through a
maze, climbs stairs, dig holes and dodges enemies in order to collect all the
gold nuggets and escape to the next level.  Your enemies are also after the
gold.  Worse still, they are after you!.

KGoldrunner is a fast-paced platform game where the player must navigate a
maze while collecting gold nuggets and avoiding enemies.  A variety of level
packs are included, as well as an editor to create new levels.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kdiamond
  (package
    (name "kdiamond")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kdiamond-" version ".tar.xz"))
      (sha256
       (base32 "1d3c4pckddnri9i19g2pi2ygpqakllrgy6azgvnh5hn20kgsw7d9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           knotifications
           knotifyconfig
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kdiamond/")
    (synopsis "Three-in-a-row game")
    (description "KDiamond is a three-in-a-row game like Bejeweled.  It
features unlimited fun with randomly generated games and five difficulty
levels with varying number of diamond colors and board sizes.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kfourinline
  (package
    (name "kfourinline")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kfourinline-" version ".tar.xz"))
      (sha256
       (base32 "0rb5jcmmf19bidwywj56dn0wfrnrfi5kc75c20d7mxnlgygfdnkg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdnssd
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kfourinline/")
    (synopsis "Place 4 pieces in a row")
    (description "KFourInLine is a board game for two players based on the
Connect-Four game.

KFourInLine is a game where two players take turns dropping pieces into a
grid, the winner being the first to place four pieces in a line.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kblackbox
  (package
    (name "kblackbox")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kblackbox-" version ".tar.xz"))
      (sha256
       (base32 "10j8rnpr3gjaqspx4mxqj9cncqj6v2jn5rkldr46bv7yxgjb5rw3"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kblackbox/")
    (synopsis "Find atoms in a grid by shooting electrons")
    (description "KBlackbox is a game of hide and seek played on a grid of
boxes where the computer has hidden several balls.  The position of the hidden
balls can be deduced by shooting beams into the box

KBlackBox is a game of hide and seek played on an grid of boxes, where the
player shoots rays into the grid to deduce the positions of hidden objects.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public knetwalk
  (package
    (name "knetwalk")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/knetwalk-" version ".tar.xz"))
      (sha256
       (base32 "0ahms3imvkdknp1z2h6j42k9g1i20ygd2633icjv37d2cbij128m"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/knetwalk/")
    (synopsis "Turn the board pieces to get all computers connected")
    (description "KNetWalk is a small game where you have to build up a
computer network by rotating the wires to connect the terminals to the server.
When the network is build, a highscore-list comes up where competitions can be
fought out.

KNetwalk is a puzzle game where the player arranges sections of wire to
connect all the computers on the board.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public bomber
  (package
    (name "bomber")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bomber-" version ".tar.xz"))
       (sha256
        (base32 "1mlxs2dbsycq7mw9g1hl2l17gl0z33mrry5r0zmz74i67nfijg8w"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/bomber/")
    (synopsis "Arcade bombing game")
    (description "Bomber is a single player arcade game.

The player is invading various cities in a plane that is decreasing in height.
The goal of the game is to destroy all the buildings and advance to the next
level.  Each level gets a bit harder by increasing the speed of the plane and
the height of the buildings.

Bomber is a game where you fly a spaceship and attempt to bomb the buildings
below you.  Each pass the spaceship makes, it gets lower and lower.  If you've
not destroyed a building in your path, you will crash into it.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public granatier
  (package
    (name "granatier")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/granatier-" version ".tar.xz"))
      (sha256
       (base32 "16yriharl66frglmdy6750nixczh0l4c19nnr6dav15m8qfb3g6b"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           knewstuff
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/granatier/")
    (synopsis "Bomberman clone")
    (description "Granatier is a clone of the classic Bomberman game,
inspired by the work of the Clanbomber clone.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksirk
  (package
    (name "ksirk")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksirk-" version ".tar.xz"))
       (sha256
        (base32 "1ipnkg2mgj37g5s5ihlys176kn2c11f3d57xr9zhqf8fvkvrkfm0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           ki18n
           kiconthemes
           kio
           knewstuff
           kwallet
           kxmlgui
           libkdegames
           phonon
           qca
           qtbase-5
           qtdeclarative-5
           qtsvg-5
           zlib))
    (home-page "https://apps.kde.org/ksirk/")
    (synopsis "Computerized version of the well known strategy board game
'Risk'")
    (description "KsirK is a multi-player network-enabled game.  The goal of
the game is simply to conquer the world by attacking your neighbors with your
armies.

At the beginning of the game, countries are distributed to all the players.
Each country contains one army represented by an infantryman.  Each player has
some armies to distribute to his countries.  On each turn, each player can
attack his neighbours, eventually conquering one or more countries.  At the
end of each turn, some bonus armies are distributed to the players in function
of the number of countries they own.  The winner is the player that conquered
all the world.

Features:
@itemize
@item Support for 1-6 human or computer players
@item Multi-player gaming over a network
@item You can easily create new skins with SVG graphics and the skin editor
@item Hot New Stuff support.  You can easily download and install new skins
@end itemize

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public palapeli
  (package
    (name "palapeli")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/palapeli-" version ".tar.xz"))
       (sha256
        (base32 "076igql89sx55hfxjb79248ih4cjbkr1s1jnz46y3dk793rscm8g"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kcrash
           ki18n
           ki18n
           kio
           kitemviews
           knotifications
           kservice
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5
           shared-mime-info))
    (home-page "https://apps.kde.org/palapeli/")
    (synopsis "Jigsaw puzzle game")
    (description "Palapeli is a jigsaw puzzle game.  Unlike other games in
that genre, you are not limited to aligning pieces on imaginary grids.  The
pieces are freely moveable.  Also, Palapeli features real persistency, i.e.
everything you do is saved on your disk immediately.

Palapeli is the Finnish word for jigsaw puzzle.

This package is part of the KDE games module.")
    (license license:gpl2+)))

(define-public kiriki
  (package
    (name "kiriki")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kiriki-" version ".tar.xz"))
       (sha256
        (base32 "0qbm0zjjqnbcdm39zi8h240nblpa1pa7g1ls9mghzbqrdrh7n3a0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kiriki/")
    (synopsis "Yahtzee dice game")
    (description "Kiriki is an addictive and fun dice game, designed to be
played by as many as six players.

Participants have to collect points by rolling five dice for up to three times
per single turn to make combinations with the highest score.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kigo
  (package
    (name "kigo")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kigo-" version ".tar.xz"))
       (sha256
        (base32 "14pp73b9mbf0ny75b90vs7z9l61m7zp8cll7hl4bplqh1kig1szf"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           knewstuff
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kigo/")
    (synopsis "Go board game")
    (description "Kigo is an open-source implementation of the popular Go
game.

Go is a strategic board game for two players.  It is also known as
igo (Japanese), weiqi or wei ch'i (Chinese) or baduk (Korean).  Go is noted
for being rich in strategic complexity despite its simple rules.  The game is
played by two players who alternately place black and white stones (playing
pieces, now usually made of glass or plastic) on the vacant intersections of a
grid of 19x19 lines (9x9 or 13x13 for easier games).

You also need to install a go engine, e.g. @code{gnugo}.

This package is part of the KDE games module.")
    (license license:gpl3+)))

(define-public kubrick
  (package
    (name "kubrick")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kubrick-" version ".tar.xz"))
       (sha256
        (base32 "0hx81cp1lql74c9067dw7mi78c6sp6p1a035j2nzjn9drpxal6p2"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list glu
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kubrick/")
    (synopsis "Game based on Rubik's Cube")
    (description "Kubrick is a game based on the Rubik's Cube puzzle.

The cube sizes range from 2x2x2 up to 6x6x6, or you can play with irregular
\"bricks\" such as 5x3x2 or \"mats\" such as 6x4x1 or 2x2x1.  The game has a
selection of puzzles at several levels of difficulty, as well as demos of
pretty patterns and solution moves, or you can make up your own puzzles.  The
game has unlimited undo, redo, save and reload capabilities.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public lskat
  (package
    (name "lskat")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/lskat-" version ".tar.xz"))
       (sha256
        (base32 "1cfs1lfwgxwpn2g56y7jb2c6ijd81bi8ba8ap0yyx6nhv6na072b"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kguiaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/lskat/")
    (synopsis "Lieutnant Skat card game")
    (description "Lieutnant Skat (from German \"Offiziersskat\") is a fun and
engaging card game for two players, where the second player is either live
opponent, or a built in artificial intelligence.

Lieutnant Skat is a simplified variant of the Skat card game for two players.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kapman
  (package
    (name "kapman")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kapman-" version ".tar.xz"))
      (sha256
       (base32 "1974z7g3ylvf48xh3xhf3gr7iphgmj83ir9hss1a2ba0hpgg463k"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kapman/")
    (synopsis "Pac-Man clone")
    (description "Kapman is a clone of the well known game Pac-Man.

You must run through the maze to eat all pills without being captured by a
ghost.  By eating an energizer, Kapman gets the ability to eat ghosts for a
few seconds.  When a stage is cleared of pills and energizer the player is
taken to the next stage with slightly increased game speed

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kspaceduel
  (package
    (name "kspaceduel")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kspaceduel-" version ".tar.xz"))
       (sha256
        (base32 "0dv539jlpkj8hr4cz0ncqm3scg6ja3s41p37bpqd94zicfvzxw84"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kspaceduel/")
    (synopsis "Two player game with shooting spaceships flying around a sun")
    (description "KSpaceduel is a space battle game for one or two players,
where two ships fly around a star in a struggle to be the only survivor.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public bovo
  (package
    (name "bovo")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bovo-" version ".tar.xz"))
       (sha256
        (base32 "1jzvazqy5vcwkyhnbzw7sh8ngff5clclq98vbbhzd9dmnacirdbq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/bovo/")
    (synopsis "Classic pen and paper game: five in a line")
    (description "Bovo is a Gomoku (from Japanese 五目並べ - lit.  \"five
points\") like game for two players, where the opponents alternate in placing
their respective pictogram on the game board.  The winner is the first to
complete a line of five markers.  (Also known as: Connect Five, Five in a row,
X and O, Naughts and Crosses)

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public killbots
  (package
    (name "killbots")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/killbots-" version ".tar.xz"))
       (sha256
        (base32 "1ncr55xq04vrx6bss1ahk86c3l9ckhv4zjbc6gq4krhjw0lkdfiv"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/killbots/")
    (synopsis "Port of the classic BSD console game robots")
    (description "Killbots is a simple game of evading killer robots.

Who created the robots and why they have been programmed to destroy, no one
knows.  All that is known is that the robots are numerous and their sole
objective is to destroy you.  Fortunately for you, their creator has focused
on quantity rather than quality and as a result the robots are severely
lacking in intelligence.  Your superior wit and a fancy teleportation device
are your only weapons against the never-ending stream of mindless automatons.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksnakeduel
  (package
    (name "ksnakeduel")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksnakeduel-" version ".tar.xz"))
      (sha256
       (base32 "06rill73xhhxra7kmbvwwriv9vbi91641z334ry1m4rr1qm2cdd6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/ksnakeduel/")
    (synopsis "Snake race played against the computer")
    (description "KSnakeDuel is a fast action game where you steer a snake
which has to eat food.  While eating the snake grows.  But once a player
collides with the other snake or the wall the game is lost.  This becomes of
course more and more difficult the longer the snakes grow.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kollision
  (package
    (name "kollision")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kollision-" version ".tar.xz"))
      (sha256
       (base32 "0avin6s1lglfps6qlvz19i27nb0x0hgrl4q2brpq4kax7azs1nc3"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/kollision/")
    (synopsis "Simple ball dodging game")
    (description "In Kollision you use mouse to control a small blue ball in a
closed space environment filled with small red balls, which move about
chaotically.  Your goal is to avoid touching any of those red balls with your
blue one, because the moment you do the game will be over.  The longer you can
stay in game the higher will your score be.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public knavalbattle
  (package
    (name "knavalbattle")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/knavalbattle-" version ".tar.xz"))
      (sha256
       (base32 "1mpj1783za6b7a7cjawy4v0z24dvcd34gdb25qch4gi9cx1lc28z"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/knavalbattle/")
    (synopsis "Battleship board game with built-in game server")
    (description "KBattleship is a Battle Ship game for KDE.

Ships are placed on a board which represents the sea.  Players try to hit each
others ships in turns without knowing where they are placed.  The first player
to destroy all ships wins the game.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kreversi
  (package
    (name "kreversi")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kreversi-" version ".tar.xz"))
      (sha256
       (base32 "0lbypkh6lc5af43c2p19gs2c53icxd26abxf5rhs2c8182gr39b8"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           kiconthemes
           kio
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kreversi/")
    (synopsis "Old reversi board game, also known as othello")
    (description "KReversi is a simple one player strategy game played
against the computer.

If a player's piece is captured by an opposing player, that piece is turned
over to reveal the color of that player.  A winner is declared when one player
has more pieces of his own color on the board and there are no more possible
moves.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksquares
  (package
    (name "ksquares")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksquares-" version ".tar.xz"))
      (sha256
       (base32 "1wbrakq1wnwp558y140j9vbid3g0k332rwbilky7z11c0giiv76x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5))
    (home-page "https://apps.kde.org/ksquares/")
    (synopsis "Dots and Boxes game")
    (description "KSquares is an implementation of the popular paper based
game Squares.  Two players take turns connecting dots on a grid to complete
squares, the player with the most squares wins.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kjumpingcube
  (package
    (name "kjumpingcube")
    (version "21.12.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kjumpingcube-" version ".tar.xz"))
      (sha256
       (base32 "1wlk6my6pawmdv3zgcpnyyzpjwz0wii0h8i1z0gxhbpg9nc8iy1r"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative-5
           qtsvg-5))
    (home-page "https://apps.kde.org/kjumpingcube/")
    (synopsis "Simple tactical game for number-crunchers")
    (description "KJumpingcube is a simple tactical game for one or two
players, played on a grid of numbered squares.  Each turn, players compete for
control of the board by capturing or adding to one square.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))
