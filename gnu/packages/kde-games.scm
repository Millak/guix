;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2024, 2025 Zheng Junjie <873216071@qq.com>
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
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module ((gnu packages freedesktop) #:select (shared-mime-info))
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xiph))

(define-public ktuberling
  (package
    (name "ktuberling")
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktuberling-" version ".tar.xz"))
       (sha256
        (base32 "0hdn2rvi1v6jzvidqq6db38axiah7cg1jsmj0bdlpfjzbffi8mjl"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
                                        ;python-wrapper
           qtmultimedia
           qtdeclarative
           qtsvg))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/picmi-" version ".tar.xz"))
       (sha256
        (base32 "1s2841rii2zdv6dg7987rgli28gczjsrglq19wxzg78gshhhjdbg"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           kiconthemes
           kio
           knewstuff
           kxmlgui
           libkdegames
           qtdeclarative
           qtsvg))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolf-" version ".tar.xz"))
       (sha256
        (base32 "0pbq1v84j8jl64p4lv60x1gss28181594vlhcpqyjadxwgy5n37m"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kwidgetsaddons
           kxmlgui
           ktextwidgets
           libkdegames
           qtdeclarative))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/libkmahjongg-" version ".tar.xz"))
       (sha256
        (base32 "0h3mvzqp00g9x21v6bxa04p12hkvb2zl6bg0zbkig53hxaiy775s"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tileset-dir
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Add "$out/share" to XDG_DATA_DIRS so that the default tileset
              ;; is always available.
              (substitute* "src/kmahjonggtileset.cpp"
                (("d->buildElementIdTable\\(\\);")
                 (format #f "QByteArray x = qgetenv(\"XDG_DATA_DIRS\");
if (!x.isEmpty()) {
  QString datadirs = QString::fromLocal8Bit(x) + QLatin1String(\":~a\");
  qputenv(\"XDG_DATA_DIRS\", datadirs.toLocal8Bit());
}
d->buildElementIdTable();"
                         (string-append
                          (assoc-ref outputs "out") "/share")))))))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kauth
           kcompletion
           kcodecs
           kconfigwidgets
           kcoreaddons
           ki18n
           kwidgetsaddons
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Shared library for kmahjongg and kshisen")
    (description "Shared library and common files for kmahjongg, kshisen and
other Mah Jongg like games.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmahjongg
  (package
    (name "kmahjongg")
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kmahjongg-" version ".tar.xz"))
       (sha256
        (base32 "0cwxn49isb2c31462irng83rg82v18cvyd9cz8i6ais2imvls2r2"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kshisen-" version ".tar.xz"))
       (sha256
        (base32 "12rw0wqzr8z2khy6yim6khlfgyxrhx5mx5f41xq8hmsd4125z0sz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           kdoctools))
    (inputs
     (list kauth
           kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kxmlgui
           libkdegames
           libkmahjongg
           qtdeclarative))
    (arguments (list #:qtbase qtbase))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kajongg-" version ".tar.xz"))
       (sha256
        (base32 "0pq0vzxy7n64hjqx8hkn7gl58ligfmcnbk1mjnh9ndby8ar9pmz7"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/mjresource.py"
                (("'share', 'kmahjongglib'" all)
                 (string-append "'" (assoc-ref inputs "libkmahjongg")
                                "/share', 'kmahjongglib'")))
              (substitute* "src/sound.py"
                (("oggBinary = 'ogg123'")
                 (format #f "oggBinary = '~a'"
                         (search-input-file inputs "bin/ogg123"))))
              (substitute* "src/common.py"
                (("interpreterName = 'python3'")
                 (format #f "interpreterName = '~a'"
                         (search-input-file inputs "bin/python3"))))))
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
           kdoctools))
    (inputs
     (list bash-minimal
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           libkmahjongg
           python
           python-pyqt
           python-twisted
           python-qtpy
           python-zope-interface
           qtsvg
           vorbis-tools))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kbreakout-" version ".tar.xz"))
       (sha256
        (base32 "00qbyv7awf70dxid2m71izsl39mfvjgzrfqdpfgkgisfvrwwnw90"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtdeclarative))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmines-" version ".tar.xz"))
       (sha256
        (base32 "0qw460yp5g1ca21dl72mkdl7fidcbzs8ajvdsqyylrslh4aafxjq"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/konquest-" version ".tar.xz"))
       (sha256
        (base32 "0d5v7f851ar2psyiva0gfhhp8c89rz63vq5hy0gasm39mxfhii03"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtscxml
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kbounce-" version ".tar.xz"))
       (sha256
        (base32 "0lnyqnbxi6gifmxifnbm3a3ss6a45872vb3vphd7g4g8dp2fxckx"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kblocks-" version ".tar.xz"))
       (sha256
        (base32 "11spx4wq8bz2r00xrr36ipq4g5mw7hf8my4ax2armgqjx0sldjyq"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/ksudoku-" version ".tar.xz"))
       (sha256
        (base32 "0d1589bp97d5ljlyv6938lcj484c7ah53m6593ngdrmln4zydwia"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           glu
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/klines-" version ".tar.xz"))
       (sha256
        (base32 "1s4b1vabygiqcjvlxj0j7xzk0f9r0rwkk1lhzyy5fx6qzyakbmry"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kgoldrunner-" version ".tar.xz"))
       (sha256
        (base32 "0wnr839axw0q0690wvvz0c09ynmdyh7bi27fayzgp0gkiq1cnmj9"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtbase
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdiamond-" version ".tar.xz"))
       (sha256
        (base32 "1ljyjpar1g937xk70cb0ignpyljy3zxk1037zy3w68nil48823mi"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kfourinline-" version ".tar.xz"))
       (sha256
        (base32 "0bxm5637c2czhp888ipgl8bx5w6xvwng5577nnf1c94qy3gmbbsk"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           kxmlgui
           libkdegames
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kblackbox-" version ".tar.xz"))
       (sha256
        (base32 "1vsgs6frnhxif7s9lnlpj6vf6m7szqavwciycwn8mbv39hpj8mjl"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/knetwalk-" version ".tar.xz"))
       (sha256
        (base32 "1dm3ixrx3zhzrbvwjy9l2g9zfz5ap5zj0bp5wpd67n33j2nlaqwg"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bomber-" version ".tar.xz"))
       (sha256
        (base32 "03llwmd8ichdb9n2xy0rhxy3hwzrxq39jmspjcynxnapl8qq2jnh"))))
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
           qtdeclarative))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/granatier-" version ".tar.xz"))
       (sha256
        (base32 "1xmh7w6q18g4vhqzyd0s63yznh67k1nr16fqc1zrcqa532phgwyc"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/granatier/")
    (synopsis "Bomberman clone")
    (description "Granatier is a clone of the classic Bomberman game,
inspired by the work of the Clanbomber clone.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksirk
  (package
    (name "ksirk")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksirk-" version ".tar.xz"))
       (sha256
        (base32 "182rvi085jl5rdvfzrkd8pymvbadkwk12p8ymljn9fcpij4jx6jd"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           knewstuff
           kwallet
           kxmlgui
           libkdegames
           phonon
           qca-qt6
           qt5compat
           qtdeclarative
           qtmultimedia
           qtsvg
           zlib))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/palapeli-" version ".tar.xz"))
       (sha256
        (base32 "0878cdqasj45m7k7xafak4b0y5q5pw4yryyxnnsiv8r6qsaki48h"))))
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
           kdbusaddons
           ki18n
           kio
           kitemviews
           knotifications
           kservice
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtdeclarative
           qtsvg
           shared-mime-info))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kiriki-" version ".tar.xz"))
       (sha256
        (base32 "0r3za1787j1cfcpyx4h3gds8nwfaxrwai3jjd5lfd9zv9cqzky84"))))
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
           qtdeclarative))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kigo-" version ".tar.xz"))
       (sha256
        (base32 "0nwkslvww3ic1ki86shpmf03ppdid612qqi68r7cixmfdcbykx8i"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-gnugo-command
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "src/kigo.kcfg"
                         (("\"gnugo\"")
                          (format #f "~s"
                                  (search-input-file inputs "bin/gnugo")))))))))
    (native-inputs
     (list extra-cmake-modules
           kdoctools))
    (inputs
     (list gnugo
           kconfig
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
           qtdeclarative
           qtsvg))
    (home-page "https://apps.kde.org/kigo/")
    (synopsis "Go board game")
    (description "Kigo is an implementation of the popular Go game.

Go is a strategic board game for two players.  It is also known as
igo (Japanese), weiqi or wei ch'i (Chinese) or baduk (Korean).  Go is noted
for being rich in strategic complexity despite its simple rules.  The game is
played by two players who alternately place black and white stones (playing
pieces, now usually made of glass or plastic) on the vacant intersections of a
grid of 19x19 lines (9x9 or 13x13 for easier games).

This package is part of the KDE games module.")
    (license license:gpl3+)))

(define-public kubrick
  (package
    (name "kubrick")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kubrick-" version ".tar.xz"))
       (sha256
        (base32 "0qladyc4ivnss0hqiac4k0rjrywybhx4krl4r5cz6dh840wv2hlg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list glu
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/lskat-" version ".tar.xz"))
       (sha256
        (base32 "112nvccxrpv1wmkbzlwa000mwm6s0pz3g1b8d2lc8086cfb6d67p"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kapman-" version ".tar.xz"))
       (sha256
        (base32 "04gjig5xryiy2r5avc040422pggbz4hin6dpbf7g5r2b26i4dclv"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kspaceduel-" version ".tar.xz"))
       (sha256
        (base32 "1kjyls2g7qzds0s9gnzwww92m7fv1l08hjp77hb9mpf9a1rzxr6q"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/kspaceduel/")
    (synopsis "Two player game with shooting spaceships flying around a sun")
    (description "KSpaceduel is a space battle game for one or two players,
where two ships fly around a star in a struggle to be the only survivor.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public bovo
  (package
    (name "bovo")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bovo-" version ".tar.xz"))
       (sha256
        (base32 "1fy4jpawzlkrqsyyi8m6jn7kisha5nnl8q6fhmwx0lyv033j5d4m"))))
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
           qtdeclarative
           qtsvg))
    (arguments (list #:qtbase qtbase))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/killbots-" version ".tar.xz"))
       (sha256
        (base32 "1d86h8l5gl3x240k86iri9wg6k9km9warcnapf3579hiwaw37bfl"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksnakeduel-" version ".tar.xz"))
       (sha256
        (base32 "0f2ihrdqggzggigz86nnrfhcmkg9n9zj16az1ymndgsdlx0r168w"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kollision-" version ".tar.xz"))
       (sha256
        (base32 "1ikmqa80v9j0qdb7vd4fpx5v6baiaia9ggwhhdkwb94y098yly8a"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/knavalbattle-" version ".tar.xz"))
       (sha256
        (base32 "1a5yya6dv29a1ymqycpam0ckbs2w5kja75c112bdwlmb3wf97g3p"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kreversi-" version ".tar.xz"))
       (sha256
        (base32 "15wmrh8qhycnhr11ky9ipjr8wpwk25zfi2q4zvmxwq7c6530hjqa"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksquares-" version ".tar.xz"))
       (sha256
        (base32 "1jzd9jb9s948zb12r1r27s75kmq3wwzzw4zikgpdcf05lfif03gl"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative))
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
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kjumpingcube-" version ".tar.xz"))
       (sha256
        (base32 "00szzbrapg4h9fy66wi8fmz5in06lzqm2g2zcakiijhfd631aj22"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           qtdeclarative
           qtsvg))
    (home-page "https://apps.kde.org/kjumpingcube/")
    (synopsis "Simple tactical game for number-crunchers")
    (description "KJumpingcube is a simple tactical game for one or two
players, played on a grid of numbered squares.  Each turn, players compete for
control of the board by capturing or adding to one square.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public knights
  (package
    (name "knights")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/knights-" version ".tar.xz"))
       (sha256
        (base32 "0y99ahmv7f2lcn2krknl95vbb9d4sn78dgxkd35b3p374kcv5rxr"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kplotting
           ksvg
           ktextwidgets
           kwallet
           kxmlgui
           libkdegames
           libplasma
           qtspeech
           qtsvg
           qtwayland))
    (home-page "https://apps.kde.org/knights/")
    (synopsis "Chess game")
    (description "KNights is a chess game, featuring:

@itemize
@item local play between two players on the same computer
@item play against any computer program that supports the XBoard protocol
@item play on the Free Internet Chess Server (FICS)
@item watching two computer engines play against each other
@item support for legal move checking
@item markers for possible moves, opponent’s last move and sources of check
@item board borders and site notations
@item complete time control, with Plasma-styled clocks
@item several themes, with the possibility of downloading new ones from within
 the program
@item animated moves (configurable)
@item views for playing on a chess server, including a seek graph, text
 console, and chat widget
@item option to undo and redo moves
@item graphic interface for making and receiving offers from remote players
@item support for the UCI protocol (used by Stockfish, Chessbase, and many
 others)
@item support for saving and loading Portable game notation (PGN) files
@item use of Jovie, the KDE text-to-speak program, to speak opponent’s moves
@item ability to control the program via a D-Bus interface
@end itemize")
    (license license:gpl2+)))

(define-public kde-games
  (package
    (name "kde-games")
    (version "24.12.3")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     ;; TODO: kpat, klickety, katomic.
     (list bomber
           bovo
           granatier
           kajongg
           kapman
           kblackbox
           kblocks
           kbounce
           kbreakout
           kdiamond
           kfourinline
           kgoldrunner
           kigo
           killbots
           kiriki
           kjumpingcube
           klines
           kmahjongg
           kmines
           knavalbattle
           knetwalk
           knights
           kolf
           kollision
           konquest
           kreversi
           kshisen
           ksirk
           ksnakeduel
           kspaceduel
           ksquares
           ksudoku
           ktuberling
           kubrick
           lskat
           palapeli
           picmi))
    (home-page "https://apps.kde.org/categories/games/")
    (synopsis "KDE Games")
    (description "This metapackage includes a collection of games provided
with the official release of KDE.")
    (license
     (list license:gpl2+ license:lgpl2.0+ license:gpl3+ license:fdl1.2+))))
