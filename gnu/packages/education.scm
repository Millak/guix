;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages education)
  #:use-module (ice-9 regex)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks) ; extra-cmake-modules
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public gcompris
  (package
    (name "gcompris")
    (version "17.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://gcompris.net/download/gtk/src/gcompris-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "18y483alb4r4vfmh80nnl0pah5gv0b8frcm6l1drb9njn5xlcpgc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; Use SDL mixer because otherwise GCompris would need an old version
       ;; of Gstreamer.
       (list "--enable-sdlmixer"
             "LDFLAGS=-lgmodule-2.0")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-mixer")
                                    "/include/SDL:"
                                    (or (getenv "CPATH") "")))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("sdl-mixer" ,sdl-mixer)
       ("sqlite" ,sqlite)
       ("glib:bin" ,glib)
       ("python" ,python)))
    (native-inputs
     `(("intltool" ,intltool)
       ("texinfo" ,texinfo)
       ("texi2html" ,texi2html)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "http://gcompris.net")
    (synopsis "Educational software suite")
    (description "GCompris is an educational software suite comprising of
numerous activities for children aged 2 to 10.  Some of the activities are
game orientated, but nonetheless still educational.  Below you can find a list
of categories with some of the activities available in that category.

@enumerate
@item computer discovery: keyboard, mouse, different mouse gestures, ...
@item arithmetic: table memory, enumeration, double entry table, mirror image, ...
@item science: the canal lock, the water cycle, the submarine, electric simulation ...
@item geography: place the country on the map
@item games: chess, memory, connect 4, oware, sudoku ...
@item reading: reading practice
@item other: learn to tell time, puzzle of famous paintings, vector drawing, cartoon making, ...
@end enumerate
")
    (license license:gpl3+)))

(define-public gcompris-qt
  (package
    (name "gcompris-qt")
    (version "0.96")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gcompris.net/download/qt/src/gcompris-qt-"
             version ".tar.xz"))
       (sha256
        (base32 "06483il59l46ny2w771sg45dgzjwv1ph7vidzzbj0wb8wbk2rg52"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 &"))
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gcompris-qt")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtdeclarative" "qtmultimedia" "qtsvg")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtdeclarative" "qtgraphicaleffects"
                           "qtmultimedia" "qtquickcontrols"))))
               #t))))
       #:configure-flags (list "-DQML_BOX2D_MODULE=disabled"
                               "-DBUILD_TESTING=TRUE")))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("openssl" ,openssl)
       ("python-2" ,python-2)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtsensors" ,qtsensors)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://gcompris.net/index-en.html")
    (synopsis "Educational games for small children")
    (description
     "Gcompris offers a large collection of educational games for small
children, designed to be a unified interface to integrate more educational
games.  Language-oriented games contain vocabulary, sounds, and voices for
many different languages.
Currently available boards include:
@enumerate
@item learning how to use a mouse and keyboard
@item learning simple arithmetic
@item learning how to read an analog clock
@item recognize letters after hearing their names
@item reading practice
@item small games (memory games, jigsaw puzzles, ...)
@end enumerate\n")
    (license license:gpl3+)))

(define-public tipp10
  (package
    (name "tipp10")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              ;; guix download is not able to handle the download links on the
              ;; home-page, which use '<meta http-equiv="refresh" …>'
              (uri (string-append "mirror://debian/pool/main/"
                                  "t/tipp10/tipp10_2.1.0.orig.tar.gz"))
              (sha256
               (base32
                "0d387b404j88gsv6kv0rb7wxr23v5g5vl6s5l7602x8pxf7slbbx"))
              (patches (search-patches "tipp10-fix-compiling.patch"
                                       "tipp10-remove-license-code.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; packages has no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-new-version-check
           (lambda _
             ;; Make new version check to default to false.
             ;; TODO: Remove the checkbox from the dialog and the check itself
             (substitute* '("widget/settingspages.cpp" "widget/mainwindow.cpp")
               (("settings.value(\"check_new_version\", true)")
                "settings.value(\"check_new_version\", false)"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Make program honor $PREFIX
               (substitute* "tipp10.pro"
                 (("\\.path = /usr/") (string-append ".path = " out "/")))
               (substitute* "def/defines.h"
                 (("\"/usr/") (string-append "\"" out "/")))
               ;; Recreate Makefile
               (invoke "qmake")))))))
    (inputs
     `(("qt4" ,qt-4)
       ("sqlite" ,sqlite)))
    (home-page "https://www.tipp10.com/")
    (synopsis "Touch typing tutor")
    (description "Tipp10 is a touch typing tutor.  The ingenious thing about
the software is its intelligence feature: characters that are mistyped are
repeated more frequently.  Beginners will find their way around right away so
they can start practicing without a hitch.

Useful support functions and an extensive progress tracker, topical lessons
and the ability to create your own practice lessons make learning to type
easy.")
    (license license:gpl2)))

(define-public snap
  (package
    (name "snap")
    (version "5.3.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmoenig/Snap.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lm5vvykdzgn667kvnsv0ab5bl0kjsr05kvcd18a7pn0g8sykfpc"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (share (string-append out "/share/snap")))
           (copy-recursively source share)
           ;; Replace the sole minified file in the package.
           (with-directory-excursion (string-append share "/src")
             (delete-file "FileSaver.min.js")
             (symlink (string-append (assoc-ref %build-inputs "js-filesaver")
                                     "/share/javascript/FileSaver.min.js")
                      "FileSaver.min.js"))
           ;; Create a "snap" executable.
           (let* ((bin (string-append out "/bin"))
                  (script (string-append bin "/snap"))
                  (snap (string-append share "/snap.html"))
                  (bash (string-append (assoc-ref %build-inputs "bash")
                                       "/bin/sh"))
                  (xdg-open (string-append (assoc-ref %build-inputs "xdg-utils")
                                           "/bin/xdg-open")))
             (mkdir-p bin)
             (call-with-output-file script
               (lambda (port)
                 (format port "#!~a\n~a '~a'" bash xdg-open snap)))
             (chmod script #o555)))
         #t)))
    (inputs
     `(("bash" ,bash-minimal)
       ("js-filesaver" ,js-filesaver)
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://snap.berkeley.edu")
    (synopsis "Visual, blocks based programming language")
    (description "Snap! (formerly BYOB) is a visual, drag-and-drop
programming language.  It is an extended reimplementation of Scratch (a
project of the Lifelong Kindergarten Group at the MIT Media Lab) that
allows you to Build Your Own Blocks.  It also features first class
lists, first class procedures, and continuations.  These added
capabilities make it suitable for a serious introduction to computer
science for high school or college students.

This package provides a @command{snap} executable calling @command{xdg-open}
to open the application in a web browser, for offline usage.")
    (license license:agpl3+)))

(define-public toutenclic
  (package
    (name "toutenclic")
    (version "7.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bipede.fr/downloads/logiciels/"
                           "ToutEnClic-" version "-src.zip"))
       (sha256
        (base32 "0xg24p925rl5bfqsq3jb2lrkidb0f3kbmay5iyxxmjsn3ra0blyh"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/toutenclic"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (doc (string-append out "share/doc/" ,name "-" ,version))
                    (bin (string-append out "/bin"))
                    (executable "toutenclic"))
               ;; Install icon.
               (install-file "toutenclic.png" pixmaps)
               ;; Move files into "share/" directory.
               (for-each (lambda (f) (install-file f share))
                         (find-files "." "\\.py$"))
               ;; Install documentation.
               (install-file "ToutEnClic.pdf" doc)
               ;; Create executable in "bin/".
               (mkdir-p bin)
               (with-directory-excursion bin
                 (symlink (string-append share "/" executable ".py")
                          executable)))
             #t))
         (add-after 'install 'create-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (mkdir-p applications)
               (call-with-output-file
                   (string-append applications "/toutenclic.desktop")
                 (lambda (file)
                   (format file
                           "[Desktop Entry]~@
                            Name=ToutEnClic~@
                            Comment=For schooling without difference~@
                            Exec=~a/bin/toutenclic~@
                            TryExec=~@*~a/bin/toutenclic~@
                            Terminal=false~@
                            Icon=toutenclic~@
                            Type=Application~%"
                           out)))
               #t))))))
    (inputs `(("python-pyqt" ,python-pyqt)))
    (synopsis "School tools for physically disabled children")
    (description "ToutEnClic is intended to facilitate the schooling
of physically disabled children in ordinary schools.  It is both
a multi-page virtual exercise book and a kit including pencil,
scissors, glue, ruler, compass, protractor and square.  A virtual
keyboard is also available if the child does not have any other
specialized device.")
    (home-page "https://bipede.fr/contrib/")
    (license license:gpl3)))

(define-public childsplay
  (package
    (name "childsplay")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/schoolsplay/"
                    "childsplay-" version ".tgz"))
              (sha256
               (base32
                "0z7yp2swjnbz51vn2zyfnjn40jq38l5mbh15yafmx1z3vn2z1m77"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-dejavu-font
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((dejavu-dir
                     (string-append (assoc-ref inputs "font-dejavu")
                                    "/share/fonts/truetype"))
                    (dejavu-font
                     (string-append dejavu-dir
                                    "/DejaVuSansCondensed-Bold.ttf")))
               (substitute* "SPConstants.py"
                 (("^(TTF(BOLD)? = ).*" _ prefix)
                  (string-append prefix "'" dejavu-font "'\n")))
               (for-each (lambda (f) (delete-file f))
                         (find-files "lib/SPData" "DejaVu"))
               #t)))
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (share (string-append out "/share/childsplay"))
                    (doc (string-append out "/share/doc/" ,name "-",version)))
               ;; Install icon.
               (install-file "lib/SPData/themes/childsplay/logo_cp.svg" pixmaps)
               ;; Install data.
               (mkdir-p share)
               (for-each (lambda (f)
                           (copy-recursively f (string-append share "/" f)))
                         '("alphabet-sounds" "lib" "locale" "SPWidgets"))
               (for-each (lambda (f) (install-file f share))
                         (find-files "." "\\.(py|dev|db)$"))
               ;; Install documentation.
               (mkdir-p doc)
               (copy-recursively "docs" doc)
               #t)))
         (add-after 'install 'create-executable
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((python (string-append (assoc-ref inputs "python")
                                           "/bin/python"))
                    (out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (executable (string-append bin "/childsplay")))
               (mkdir-p bin)
               (call-with-output-file executable
                 (lambda (file)
                   (format file
                           "~a ~a"
                           python
                           (string-append out "/share/childsplay/childsplay.py"))))
               (chmod executable #o555)
               #t)))
         (add-after 'install 'create-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (mkdir-p applications)
               (call-with-output-file
                   (string-append applications "/childsplay.desktop")
                 (lambda (file)
                   (format file
                           "[Desktop Entry]~@
                            Name=Childsplay~@
                            Comment=Suite of educational games for young children~@
                            Comment[ca]=Conjunt de jocs educatius per a xiquets~@
                            Comment[es]=Conjunto de juegos educativos para niños~@
                            Comment[de]=Sammlung mit lehrreichen Spielen für kleine Kinder~@
                            Exec=~a/bin/childsplay.py~@
                            Terminal=false~@
                            Icon=logo_cp.svg~@
                            Type=Application~@
                            Categories=Application;Game;Education;KidsGame;~@
                            Keywords=suite;children;games;young;educational;~%"
                           out)))
               #t))))))
    (inputs
     `(("font-dejavu" ,font-dejavu)
       ("pygame" ,python2-pygame)
       ("sqlalchemy" ,python2-sqlalchemy)))
    (synopsis "Suite of educational games for young children")
    (description "Childsplay is a collection of educational activities
for young children.  Childsplay can be used at home, kindergartens and
pre-schools.  Childsplay is a fun and safe way to let young children
use the computer and at the same time teach them a little math,
letters of the alphabet, spelling, eye-hand coordination, etc.")
    (home-page "http://www.schoolsplay.org")
    (license license:gpl3+)))

(define-public fet
  (package
    (name "fet")
    (version "5.41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.lalescu.ro/liviu/fet/download/"
                                  "fet-" version ".tar.bz2"))
              (sha256
               (base32
                "0ppa5h1p0y0z8x4xpn45b0x3nl1khyh56m22v6xysk3znxlak4q7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-hardcoded-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (list "fet.pro"
                                "src/src.pro"
                                "src/src-cl.pro"
                                "src/interface/fet.cpp")
               (("/usr") (assoc-ref outputs "out")))
             #t))
         (replace 'configure
           (lambda _ (invoke "qmake" "fet.pro"))))))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://www.lalescu.ro/liviu/fet/")
    (synopsis "Timetabling software")
    (description "FET is a program for automatically scheduling the
timetable of a school, high-school or university.  It uses a fast and
efficient timetabling algorithm.

Usually, FET is able to solve a complicated timetable in maximum 5-20
minutes.  For simpler timetables, it may take a shorter time, under
5 minutes (in some cases, a matter of seconds).  For extremely
difficult timetables, it may take a longer time, a matter of hours.")
    (license license:agpl3+)))

(define-public klavaro
  (package
    (name "klavaro")
    (version "3.09")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/klavaro/klavaro-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "12gml7h45b1w9s318h0d5wxw92h7pgajn2kh57j0ak9saq0yb0wr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("curl" ,curl)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (home-page "https://klavaro.sourceforge.io/en/index.html")
    (synopsis "Touch typing tutor")
    (description
     "Klavaro is a simple tutor to teach correct typing, almost independently of
language and very flexible regarding to new or unknown keyboard layouts.")
    (license license:gpl3+)))

(define-public ktouch
  (package
    (name "ktouch")
    (version "19.08.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/applications/"
                            version "/src/ktouch-" version ".tar.xz"))
        (sha256
         (base32
          "0dm6xcwai0bx2h16rny1xa9n1509mfxvy39kfxx5qih53p15jrnk"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases (@ (guix build qt-build-system) %standard-phases)
         (add-after 'configure 'patch-makefiles
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qtdec (assoc-ref inputs "qtdeclarative")))
               (substitute* '("src/CMakeFiles/ktouch_autogen.dir/build.make"
                              "src/CMakeFiles/ktouch.dir/build.make")
                 (("/gnu/store/.*qmlcachegen")
                  (string-append qtdec "/bin/qmlcachegen"))))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kqtquickcharts" ,kqtquickcharts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxcb" ,libxcb)
       ("libxkbfile" ,libxkbfile)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://edu.kde.org/ktouch/")
    (synopsis "Touch typing tutor")
    (description
     "KTouch is an aid for learning how to type with speed and accuracy.  It
provides a sample text to type and indicates which fingers should be used for
each key.  A collection of lessons are included for a wide range of different
languages and keyboard layouts, and typing statistics are used to dynamically
adjust the level of difficulty.")
    (license license:gpl2)))
