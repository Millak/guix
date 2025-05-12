;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
;;; Copyright © 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2022 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Luis Higino <luishenriquegh2701@gmail.com>
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
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks) ; extra-cmake-modules
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public cf-tool
  (package
    (name "cf-tool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xalanq/cf-tool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ad2mljjg4pr8jjk9i1asnld16xi1wdfnh25drngm3c590cmrnfj"))
       (patches (search-patches "cf-tool-add-languages.patch"))
       (modules '((guix build utils)))
       ;; Remove assets and vendorized dependencies from checkout
       (snippet '(begin
                   (delete-file-recursively "assets")
                   (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/xalanq/cf-tool"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'add-alternate-name
            (lambda* _
              (let ((bin (string-append #$output "/bin")))
                (symlink (string-append bin "/cf-tool")
                         (string-append bin "/cf"))))))))
    (native-inputs
     (list go-github-com-docopt-docopt-go
           go-github-com-fatih-color
           go-github-com-k0kubun-go-ansi
           go-github-com-mitchellh-go-homedir
           go-github-com-olekukonko-tablewriter
           go-github-com-puerkitobio-goquery
           go-github-com-sergi-go-diff
           go-github-com-shirou-gopsutil
           go-github-com-skratchdot-open-golang
           go-golang-org-x-crypto
           go-golang-org-x-term))
    (home-page "https://github.com/xalanq/cf-tool")
    (synopsis
     "Command-line interface tool for @url{https://codeforces.com, Codeforces}")
    (description
     "Codeforces Tool is a command-line interface tool for
@url{https://codeforces.com,Codeforces}.  Its features include:
@itemize
@item support Contests, Gym, Groups and acmsguru
@item support all programming languages in Codeforces
@item submit codes
@item watch submissions' status dynamically
@item fetch problems' samples
@item compile and test locally
@item clone all codes of someone
@item generate codes from the specified template (including timestamp, author, etc.)
@item list problems' stats of one contest
@item use default web browser to open problems' pages, standings' page, etc.
@item setup a network proxy and  setup a mirror host
@end itemize")
    (license license:expat)))

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
       #:make-flags
       (list "CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "include/SDL")
                      ":" (or (getenv "CPATH") ""))))))))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("librsvg" ,(librsvg-for-system))
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
    (home-page "https://gcompris.net")
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
    (version "25.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/gcompris/qt/src/gcompris-qt-"
             version ".tar.xz"))
       (sha256
        (base32 "1my67r7x6j7snidnj47v3ndhf3i5sxn0zqj4d8apaw6mbqms96vj"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'start-xorg-server
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; The test suite requires a running X server.
                   (system "Xvfb :1 &")
                   (setenv "DISPLAY" ":1")
                   ;; The test suite wants to write to /homeless-shelter
                   (setenv "HOME" (getcwd)))))
           #:configure-flags #~(list "-DQML_BOX2D_MODULE=disabled"
                                     "-DBUILD_TESTING=TRUE")))
    (native-inputs
     (list extra-cmake-modules
           gettext-minimal
           kdoctools
           perl
           pkg-config
           qttools
           xorg-server-for-tests))
    (inputs
     (list openssl
           python-wrapper
           qtcharts
           qtdeclarative
           qtmultimedia
           qtsensors
           qtsvg))
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
@end enumerate")
    (license (list license:silofl1.1    ; bundled fonts
                   license:agpl3+))))

(define-public gotypist
  (let ((revision "0")
        (commit "03f8618f8e23acdaa94cda3bcf197da520db8dd4"))
    (package
      (name "gotypist")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KappaDistributive/gotypist")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sjndaspqfzffjxz388m384wqz5lzbiw4cwpi688k5aq7n05jh0f"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/KappaDistributive/gotypist"
         #:import-path "github.com/KappaDistributive/gotypist/v1"
         #:install-source? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'install-data
             (lambda* (#:key import-path unpack-path outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (data (string-append out "/share/gotypist/data")))
                 (with-directory-excursion "src"
                   (with-directory-excursion import-path
                     (substitute* "lesson.go"
                       (("\"data/")
                        (format #f "\"~a/" data))))
                   (with-directory-excursion unpack-path
                     (mkdir-p data)
                     (copy-recursively "data" data))))))
           (add-after 'install 'rename-executable
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (with-directory-excursion bin
                   (rename-file "v1" "gotypist"))))))))
      (native-inputs
       (list go-github-com-gizak-termui-v3
             go-github-com-stretchr-testify))
      (home-page "https://github.com/KappaDistributive/gotypist")
      (synopsis "Simple typing trainer for text terminals")
      (description
       "Gotypist is a simple typing tutor for text terminals, similar to
gtypist but with no instruction.  Hence it's best suited for people who already
know how to touch type and wish to improve their typing accuracy and/or speed.

You can provide your own lesson text, choose from the included samples, or ask
@command{gotypist} to construct a random lesson from a fixed list of the most
frequently used words in American English.")
      (license license:expat))))

(define-public tipp10
  (package
    (name "tipp10")
    (version "3.3.0")
    (source (origin
              (method git-fetch)
              ;; Use the community maintained Qt 6 fork of the project, as the
              ;; original software is now developed as a web application.  The
              ;; latest official version was 2.1.0.
              (uri (git-reference
                    (url "https://gitlab.com/tipp10/tipp10.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "138xf55csnq53mlkhj50g9bacay8kxz6p9vnzd7jyv6rq1xch5nq"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase    ;qtbase 6
                     #:tests? #f))      ;packages has no tests
    (inputs (list qtbase qtmultimedia))
    (home-page "https://www.tipp10.com/en/")
    (synopsis "Touch typing tutor")
    (description "Tipp10 is a touch typing tutor.  The ingenious thing about
the software is its intelligence feature: characters that are mistyped are
repeated more frequently.  Beginners will find their way around right away so
they can start practicing without a hitch.

Useful support functions and an extensive progress tracker, topical lessons
and the ability to create your own practice lessons make learning to type
easy.")
    ;; XXX: The LICENSE file mentions 'or later', but the source license
    ;; headers have been modified to mention only "either version 2 of the
    ;; License", which is not quite clear.
    (license license:gpl2)))

(define-public snap
  (package
    (name "snap")
    (version "7.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmoenig/Snap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13j52r810yijvkj85c356c342drc3947j28z3va7kz75mi26whsf"))))
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
             (symlink (search-input-file %build-inputs
                                         "/share/javascript/FileSaver.min.js")
                      "FileSaver.min.js"))
           ;; Create a "snap" executable.
           (let* ((bin (string-append out "/bin"))
                  (script (string-append bin "/snap"))
                  (snap (string-append share "/snap.html"))
                  (bash (search-input-file %build-inputs "/bin/sh"))
                  (xdg-open (search-input-file %build-inputs
                                               "/bin/xdg-open")))
             (mkdir-p bin)
             (call-with-output-file script
               (lambda (port)
                 (format port "#!~a\n~a '~a'" bash xdg-open snap)))
             (chmod script #o555))))))
    (inputs
     (list bash-minimal js-filesaver xdg-utils))
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
       (uri (list
             ;; XXX: Upstream does not exist anymore.
             ;; (string-append "http://www.bipede.fr/downloads/logiciels/"
             ;;                "ToutEnClic-" version "-src.zip")
             (string-append "https://archive.org/download/tout-en-clic-" version
                            "-src/ToutEnClic-" version "-src.zip")))
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
                          executable)))))
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
                           out)))))))))
    (native-inputs
     (list unzip))
    (inputs
     (list python-pyqt))
    (synopsis "School tools for physically disabled children")
    (description "ToutEnClic is intended to facilitate the schooling
of physically disabled children in ordinary schools.  It is both
a multi-page virtual exercise book and a kit including pencil,
scissors, glue, ruler, compass, protractor and square.  A virtual
keyboard is also available if the child does not have any other
specialized device.")
    (home-page "https://bipede.fr/contrib/")
    (license license:gpl3)))

(define-public openboard
  (package
    (name "openboard")
    (version "1.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenBoard-org/OpenBoard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1098pys5p82sx97xrhw54vlkn6jly0rhq8b09grmmx2h4mcpj2i2"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-initial-values
            ;; Remove useless "Check for updates" action from menu.  Also
            ;; prevent pop-up window about importing Open Sankore documents
            ;; since we don't package OpenBoard-Importer.
            (lambda _
              (substitute* "src/core/UBSettings.cpp"
                (("(appHideCheckForSoftwareUpdate = .*?)false(\\);)" _ beg end)
                 (string-append beg "true" end))
                (("(appLookForOpenSankoreInstall = .*?)true(\\);)" _ beg end)
                 (string-append beg "false" end)))))
          (add-after 'unpack 'fix-hard-coded-env
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/frameworks/UBPlatformUtils_linux.cpp"
                (("/usr/bin/env") (search-input-file inputs "/bin/env")))))
          (add-after 'unpack 'fix-library-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "OpenBoard.pro"
                (("/usr/include/quazip5")
                 (search-input-directory inputs "/include/QuaZip-Qt5-1.4/quazip"))
                (("-lquazip5")
                 "-lquazip1-qt5")
                (("/usr/include/poppler")
                 (search-input-directory inputs "/include/poppler")))))
          (replace 'configure
            (lambda _
              (invoke "qmake" "OpenBoard.pro")))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((share (string-append #$output "/share"))
                     (openboard (string-append share "/openboard"))
                     (i18n (string-append openboard "/i18n")))
                ;; Install data.
                (with-directory-excursion "resources"
                  (for-each (lambda (directory)
                              (let ((target
                                     (string-append openboard "/" directory)))
                                (mkdir-p target)
                                (copy-recursively directory target)))
                            '("customizations" "etc" "library"))
                  (mkdir-p i18n)
                  (for-each (lambda (f)
                              (install-file f i18n))
                            (find-files "i18n" "\\.qm$")))
                ;; Install desktop file an icon.
                (install-file "resources/images/OpenBoard.png"
                              (string-append share
                                             "/icons/hicolor/64x64/apps/"))
                (make-desktop-entry-file
                 (string-append share "/applications/" #$name ".desktop")
                 #:name "OpenBoard"
                 #:comment "Interactive whiteboard application"
                 #:exec "openboard %f"
                 #:icon "OpenBoard"
                 #:mime-type "application/ubz"
                 #:categories '("Education"))
                ;; Install executable.
                (install-file "build/linux/release/product/OpenBoard" openboard)
                (let ((bin (string-append #$output "/bin")))
                  (mkdir-p bin)
                  (symlink (string-append openboard "/OpenBoard")
                           (string-append bin "/openboard")))))))))
    (native-inputs
     (list qttools-5))
    (inputs
     (list alsa-lib
           coreutils-minimal            ;for patched 'env' shebang
           ffmpeg-4
           freetype
           lame
           libass
           libfdk
           libressl
           libtheora
           libva
           libvorbis
           libvpx
           libx264
           opus
           poppler
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtsvg-5
           qtwebchannel-5
           qtwebengine-5
           qtxmlpatterns
           quazip-5
           sdl
           zlib))
    (home-page "https://openboard.ch/")
    (synopsis "Interactive whiteboard for schools and universities")
    (description
     "OpenBoard is a teaching software for interactive whiteboard
designed primarily for use in schools and universities.  It can be
used both with interactive whiteboards or in a dual-screen setup with
a pen-tablet display and a beamer.")
    (license license:gpl3)))

(define-public fet
  (package
    (name "fet")
    (version "6.28.4")
    (source
     (origin
       (method url-fetch)
       (uri (let ((directory "https://www.lalescu.ro/liviu/fet/download/")
                  (base (string-append "fet-" version ".tar.bz2")))
              (list (string-append directory base)
                    (string-append directory "old/" base))))
       (sha256
        (base32 "11mcbgi8lima4fng78lqdkd5km212drkk5l4bkzz1pz1k7wcykfn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-hardcoded-directories
            (lambda _
              (substitute* (list "fet.pro"
                                 "src/src.pro"
                                 "src/src-cl.pro"
                                 "src/interface/fet.cpp")
                (("/usr") #$output))))
          (replace 'configure
            (lambda _ (invoke "qmake" "fet.pro"))))))
    (inputs
     (list qtbase))
    (home-page "https://www.lalescu.ro/liviu/fet/")
    (synopsis "Timetabling software")
    (description
     "FET is a program for automatically scheduling the timetable of a school,
high-school or university.  It uses a fast and efficient timetabling
algorithm.

Usually, FET is able to solve a complicated timetable in maximum 5-20 minutes.
For extremely difficult timetables, it may take a longer time, a matter of
hours.")
    (license license:agpl3+)))

(define-public klavaro
  (package
    (name "klavaro")
    (version "3.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/klavaro/klavaro-"
                            version ".tar.bz2"))
        (sha256
         (base32 "1avdwpmd7jmdkrwzsxd8s8qg0sqj2avcv620jvk11i81sd4pw647"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list cairo curl gtk+ gtkdatabox pango))
    (home-page "https://klavaro.sourceforge.io/en/index.html")
    (synopsis "Touch typing tutor")
    (description
     "Klavaro is a simple tutor to teach correct typing, almost independently of
language and very flexible regarding to new or unknown keyboard layouts.")
    (license license:gpl3+)))

(define-public kqtquickcharts
  (package
    (name "kqtquickcharts")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kqtquickcharts-" version ".tar.xz"))
              (sha256
               (base32
                "1hgcl062i94abq0cwz9fijyjhv2qs3fsjikiy2jb3sh73m6jydqn"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtdeclarative-5))
    (home-page "https://invent.kde.org/libraries/kqtquickcharts")
    (synopsis "QtQuick plugin to render beautiful and interactive charts")
    (description
     "This package provides a QtQuick plugin to render beautiful and interactive
charts.")
    (license (list license:lgpl2.0+ license:gpl2+))))

(define-public ktouch
  (package
    (name "ktouch")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/ktouch-" version ".tar.xz"))
       (sha256
        (base32 "1sqzm8xf3xaia0b761mgpb2q4gc8yxwhvkzwdvw0spj25irdv7n5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5 pkg-config))
    (inputs
     (list kcmutils-5
           kcompletion-5
           kconfig-5
           kconfigwidgets-5
           kcoreaddons-5
           kdeclarative-5
           ki18n-5
           kiconthemes-5
           kitemviews-5
           ktextwidgets-5
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           kqtquickcharts
           libxcb
           libxkbfile
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtquickcontrols2-5
           qtx11extras
           qtxmlpatterns))
    (home-page "https://edu.kde.org/ktouch/")
    (synopsis "Touch typing tutor")
    (description
     "KTouch is an aid for learning how to type with speed and accuracy.  It
provides a sample text to type and indicates which fingers should be used for
each key.  A collection of lessons are included for a wide range of different
languages and keyboard layouts, and typing statistics are used to dynamically
adjust the level of difficulty.")
    (license license:gpl2)))

(define-public kanatest
  ;; Latest release tarball is 0.4.8, which is really old and does not build
  ;; commit on sourceforge are not tagged, we take the latest
  (let ((commit "860e790a35f547cc96669f805d371a5ba3d8daff")
        (revision "0"))
    (package
      (name "kanatest")
      (version (git-version "0.4.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.code.sf.net/p/kanatest/code")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dz63m9p4ggzw0yb309qmgnl664qb5q268vaa3i9v0i8qsl66d78"))))
      (build-system gnu-build-system)
      (native-inputs
       (list gettext-minimal ; for msgfmt
             pkg-config))
      (inputs
       (list libxml2 gtk+))
      (home-page "https://kanatest.sourceforge.io/")
      (synopsis "Hiragana and Katakana simple flashcard tool")
      (description "Kanatest is a Japanese kana (Hiragana and Katakana) simple
flashcard tool.

During test the Kanatest displays randomly selected kana char (respecting mode
and lesson) and waits for user answer expected as romaji equivalent.  This
process continues until all questions will be answered or all questions will
be answered correctly (depends on options).  At the end of test a short info
about drilling time and correctness ratio is displayed.  The results are
stored and user can review his performance in any time.")
      (license license:gpl2+))))

(define-public anki
  (package
    (name "anki")
    ;; Later versions have dependencies on npm packages not yet in Guix.
    (version "2.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://apps.ankiweb.net/downloads/archive/anki-"
                           version "-source.tgz"))
       (sha256
        (base32 "1gfr51rnllkyzli73p4r51h5ypzfa3m7lic3m3rzpywmqwrxs07k"))
       (modules '((guix build utils)))
       ;; Fix preferences error: <https://issues.guix.gnu.org/65506>.
       (snippet '(substitute* "aqt/preferences.py"
                   (("qc\\['collapseTime']/60\\.0" x)
                    (format #f "int(~a)" x))
                   (("qc\\['timeLim']/60\\.0" x)
                    (format #f "int(~a)" x))))
       (patches (search-patches "anki-mpv-args.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ;no check target
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-update-check
           ;; Don't ‘phone home’ unasked to check for updates.
           (lambda _
             (substitute* "aqt/update.py"
               (("requests\\.post")
                "throw.an.exception.instead"))
             #t))
         (delete 'configure)            ;no configure script
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   ;; List of paths to the site-packages directories of Python
                   ;; library inputs.
                   (site-packages
                    (map (lambda (pyinput)
                           (string-append
                            (cdr pyinput)
                            "/lib/python"
                            ;; Calculate the python version to avoid breaking
                            ;; with future 3.X releases.
                            ,(version-major+minor
                              (package-version python-wrapper))
                            "/site-packages"))
                         (filter (match-lambda
                                   ((label . _)
                                    (string-prefix? "python-" label)))
                                 inputs)))
                   (qtwebengineprocess
                    (search-input-file inputs
                                       "lib/qt5/libexec/QtWebEngineProcess")))
               ;; The program fails to find the QtWebEngineProcess program, so
               ;; we set QTWEBENGINEPROCESS_PATH to help it.  PYTHONPATH is
               ;; wrapped to avoid declaring Python libraries as propagated
               ;; inputs.
               (for-each (lambda (program)
                           (wrap-program program
                             `("QTWEBENGINEPROCESS_PATH" =
                               (,qtwebengineprocess))
                             `("PATH" prefix (,(string-append
                                                (assoc-ref inputs "mpv")
                                                "/bin")))
                             `("GUIX_PYTHONPATH" = ,site-packages)))
                         (find-files bin ".")))
             #t)))))
    (native-inputs
     (list xdg-utils))
    (inputs
     `(("lame" ,lame)
       ("mpv" ,mpv)
       ("python" ,python-wrapper)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-decorator" ,python-decorator)
       ("python-distro" ,python-distro)
       ("python-jsonschema" ,python-jsonschema)
       ("python-markdown" ,python-markdown)
       ("python-pyaudio" ,python-pyaudio)
       ;; `python-pyqtwebengine' must precede `python-pyqt' in PYTHONPATH.
       ("python-pyqtwebengine" ,python-pyqtwebengine)
       ("python-pyqt" ,python-pyqt)
       ("python-requests" ,python-requests)
       ("python-send2trash" ,python-send2trash)
       ("python-sip" ,python-sip)
       ;; `qtwebengine-5' is included in `pyqtwebengine', included here for easy
       ;; wrapping.
       ("qtwebengine-5" ,qtwebengine-5)))
    (home-page "https://apps.ankiweb.net/")
    (synopsis "Powerful, intelligent flash cards")
    (description "Anki is a program which makes remembering things
easy.  Because it's a lot more efficient than traditional study
methods, you can either greatly decrease your time spent studying, or
greatly increase the amount you learn.

Anyone who needs to remember things in their daily life can benefit
from Anki.  Since it is content-agnostic and supports images, audio,
videos and scientific markup (via LaTeX), the possibilities are
endless.  For example:
@itemize
@item Learning a language
@item Studying for medical and law exams
@item Memorizing people's names and faces
@item Brushing up on geography
@item Mastering long poems
@item Even practicing guitar chords!
@end itemize")
    (license license:agpl3+)))

(define-public t4k-common
  (package
    (name "t4k-common")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tux4kids/t4kcommon")
             (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13q02xpmps9qg8zrzzy2gzv4a6afgi28lxk4z242j780v0gphchp"))
       (patches (search-patches "t4k-common-libpng16.patch"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           (substitute* "src/t4k_common.h"
             (("char wrapped_lines") "extern char wrapped_lines"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;FIXME: cannot find how to run tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":" (or (getenv "CPATH") "")))))
         (add-after 'unpack 'fix-andika-font-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/t4k_sdl.c"
               (("(/usr/share/.*?)/AndikaDesRevG\\.ttf")
                (string-append (assoc-ref inputs "font-andika")
                               "/share/fonts/truetype")))
             #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("font-andika" ,font-sil-andika)
       ("libpng" ,libpng)
       ("librsvg" ,(librsvg-for-system))
       ("libxml2" ,libxml2)
       ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net sdl-pango)))))
    (home-page "https://github.com/tux4kids/t4kcommon")
    (synopsis "Library of code shared between TuxMath and TuxType")
    (description "Tux4Kids-Common is a library of code shared between
TuxMath and TuxType.")
    (license license:gpl3+)))

(define-public tuxmath
  (package
    (name "tuxmath")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tux4kids/tuxmath")
             (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f1pz83w6d3mbik2h6xavfxmk5apxlngxbkh80x0m55lhniwkdxv"))
       (modules '((guix build utils)))
       ;; Unbundle fonts.
       (snippet
        `(begin
           ;; Remove duplicate definition.
           (substitute* "src/menu_lan.c"
             (("lan_player_type.*MAX_CLIENTS\\];") ""))
           (for-each delete-file (find-files "data/fonts" "\\.ttf$"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":"
                      (or (getenv "CPATH") "")))))
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications"))
                    (pixmaps (string-append out "/share/pixmaps")))
               (install-file "tuxmath.desktop" apps)
               (for-each (lambda (f) (install-file f pixmaps))
                         (find-files "data/images/icons/"
                                     "tuxmath\\.(png|ico|svg)$"))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("librsvg" ,(librsvg-for-system))
       ("libxml2" ,libxml2)
       ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net sdl-pango)))
       ("t4k-common" ,t4k-common)))
    (home-page "https://github.com/tux4kids/tuxmath")
    (synopsis "Educational math tutorial game")
    (description "@emph{Tux, of Math Command} is an educational math
tutorial game starring Tux, the Linux penguin, in which you play the
part of Commander Tux, as he defends his friends from an attack of
math equations.  Comets are crashing towards the friendly penguins in
their igloos, and you must destroy the comets by solving their
equations.

TuxMath also includes Factoroids, a game that gives practice in
factoring numbers and simplifying fractions, as well as zapping rocks
floating through space.")
    (license license:gpl3+)))

(define-public libeb
  (package
    (name "libeb")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "ftp://ftp.sra.co.jp/pub/misc/eb/eb-" version ".tar.bz2"))
       (sha256
        (base32
         "0psbdzirazfnn02hp3gsx7xxss9f1brv4ywp6a15ihvggjki1rxb"))))
    (build-system gnu-build-system)
    (native-inputs ; Required for building docs
     (list perl))
    (inputs
     (list zlib))
    (synopsis "C library for accessing Japanese CD-ROM books")
    (description "The EB library is a library for accessing CD-ROM
books, which are a common way to distribute electronic dictionaries in
Japan.  It supports the EB, EBG, EBXA, EBXA-C, S-EBXA and EPWING
formats.")
    ;; FIXME: I cannot find a real home page
    (home-page "https://sra.co.jp/")
    (license license:bsd-3)))

(define-public qolibri
  (package
    (name "qolibri")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url"https://github.com/ludios/qolibri")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066y7jcq9vg6hnvn7qxckzhd1qkgfzpzhw69nw5psm43qbaca8lg"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f)) ; no test target
    (native-inputs
     (list qttools-5))
    (inputs
     (list libeb
           qtbase-5
           qtmultimedia-5
           qtquickcontrols2-5
           qtdeclarative-5
           qtwebchannel-5
           qtwebengine-5
           zlib))
    (synopsis "EPWING dictionary reader")
    (description "qolibri is a dictionary viewer for the EPWING dictionary
format.  Most monolingual Japanese dictionaries can only be found in the
EPWING format.")
    (home-page "https://github.com/ludios/qolibri")
    (license license:gpl2)))

(define-public mdk
  (package
    (name "mdk")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/mdk/v" version "/mdk-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bhk3c82kyp8167h71vdpbcr852h5blpnwggcswqqwvvykbms7lb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-gui=yes" "-with-readline=yes")))
    (native-inputs
     (list flex intltool pkg-config))
    (inputs
     (list glib
           gtk+
           libglade
           ncurses
           pango
           readline))
    (home-page "https://www.gnu.org/software/mdk/manual/")
    (synopsis "Virtual development environment for Knuth's MIX")
    (description
     "GNU MDK is the Mix Development Kit, an emulation of the pedagogical
computer MIX and its assembly language MIXAL.  MIX has a virtual CPU with
standard features such as registers, memory cells, an overflow toggle,
comparison flags, input-output devices, and a set of binary instructions.
The package includes a compiler, a virtual machine, a GUI for the virtual
machine, and more.")
    (license license:gpl3+)))

(define-public exercism
  (package
    (name "exercism")
    (version "3.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exercism/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0shh84g7j977kn9kcm09rj3lz6a3y5qq9yvklsldgb9zvass5szd"))
       (patches (search-patches "exercism-disable-self-update.patch"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/exercism/cli/exercism"
       #:unpack-path "github.com/exercism/cli"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/exercism"))
                    (fish (string-append
                            out "/share/fish/vendor_completions.d/exercism.fish"))
                    (zsh  (string-append out "/share/zsh/site-functions/_exercism")))
               (mkdir-p (dirname bash))
               (with-output-to-file bash
                 (lambda ()
                   (invoke (string-append out "/bin/exercism") "completion" "bash")))
               (mkdir-p (dirname fish))
               (with-output-to-file fish
                 (lambda ()
                   (invoke (string-append out "/bin/exercism") "completion" "fish")))
               (mkdir-p (dirname zsh))
               (with-output-to-file zsh
                 (lambda ()
                   (invoke (string-append out "/bin/exercism") "completion" "zsh")))))))))
    (inputs
     (list go-github-com-blang-semver
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-spf13-viper
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://exercism.org/")
    (synopsis "Mentored learning for programming languages")
    (description "Commandline client for exercism.io, a free service providing
mentored learning for programming languages.")
    (license license:expat)))

(define-public mazo
  (package
    (name "mazo")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/luis-felipe/mazo.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14nk3qsj6lg7wp2ws8lxhb4hyjnczvw1cn9f3m466dkkfimp7ygf"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (let* ((out (assoc-ref outputs "out"))
                      (home (string-append out "/tmp")))
                 (add-installed-pythonpath inputs outputs)
                 (setenv "HOME" home)
                 (invoke "python3" "manage.py" "test")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (script (string-append bin "/mazo"))
                    (share (string-append out "/share"))
                    (help (string-append share "/help/C/mazo"))
                    (icons (string-append
                            share
                            "/icons/hicolor/scalable/apps"))
                    (apps (string-append share "/applications"))
                    (site (string-append
                           (site-packages inputs outputs)
                           "/mazo")))
               (mkdir-p help)
               (mkdir-p bin)
               (copy-file "mazo.py" script)
               (chmod script #o555)
               (install-file "icons/mazo.svg" icons)
               (install-file "lugare.ulkeva.Mazo.desktop" apps)
               (copy-recursively "help/C/mazo" help)
               (copy-recursively "mazo" site)))))))
    (native-inputs
     (list python-django
           python-django-cleanup
           python-django-svg-image-form-field
           python-pillow
           python-pycairo))
    (propagated-inputs
     (list adwaita-icon-theme
           dbus
           gstreamer
           gtk
           python
           python-django
           python-django-cleanup
           python-django-svg-image-form-field
           python-pillow
           python-pycairo
           python-pygobject
           yelp))
    (home-page "https://luis-felipe.gitlab.io/mazo/")
    (synopsis "Memorize concepts using multimedia flash cards")
    (description "Mazo is a learning application that helps you memorize
simple concepts using multimedia flash cards and spaced reviews.")
    (license license:public-domain)))

(define-public tagainijisho
  (package
    (name "tagainijisho")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Gnurou/tagainijisho/releases/download/"
             version
             "/tagainijisho-" version ".tar.gz"))
       (sha256
        (base32
         "00whfcnpn42asxmafcfbcmpwfwyv40qaqdk28psa1vp0lainmyhh"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f))                ;no test target
    (native-inputs
     (list qttools-5))
    (synopsis "Japanese dictionary and learning assistant")
    (description
     "Tagaini Jisho is a Japanese dictionary and kanji lookup tool.  It aims
at becoming your Japanese study assistant.  It allows you to quickly search
for entries and mark those that you wish to study, along with tags and
personal notes.  It also let you train entries you are studying and follows
your progression in remembering them.  Finally, it makes it easy to review
entries you did not remember by listing them on screen or printing them on
a small booklet.

Tagaini Jisho also features complete stroke order animations for more than
6000 kanji.")
    (home-page "https://www.tagaini.net")
    (license license:gpl3+)))
