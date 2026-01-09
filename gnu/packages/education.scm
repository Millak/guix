;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
;;; Copyright © 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2022 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2024 Luis Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (gnu packages cups)
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
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
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
  #:use-module (guix build-system pyproject)
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
      #:go go-1.23
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
           go-github-com-olekukonko-tablewriter-0.0.5
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
    (version "3.3.4")
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
                "1a6swdzf15jrqafwzv7grkdcl4a4nhpm8b8lh6br0djxkzqzx45b"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase    ;qtbase 6
                     #:tests? #f))      ;packages has no tests
    (inputs (list qtbase qtmultimedia qtwayland))
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
             ;; "ToutEnClic-" version "-src.zip")
             (string-append "https://archive.org/download/tout-en-clic-"
                            version "-src/ToutEnClic-" version "-src.zip")))
       (sha256
        (base32 "0xg24p925rl5bfqsq3jb2lrkidb0f3kbmay5iyxxmjsn3ra0blyh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'install
            (lambda _
              (let ((share (string-append #$output "/share/toutenclic"))
                    (bin (string-append #$output "/bin"))
                    (executable "toutenclic"))
                ;; Install icon.
                (install-file "toutenclic.png"
                              (string-append #$output "/share/pixmaps"))
                ;; Move files into "share/" directory.
                (for-each (lambda (f)
                            (install-file f share))
                          (find-files "." "\\.py$"))
                ;; Install documentation.
                (install-file "ToutEnClic.pdf"
                              (string-append #$output "share/doc/"
                                             #$name "-" #$version))
                ;; Create executable in "bin/".
                (mkdir-p bin)
                (with-directory-excursion bin
                  (symlink (string-append share "/" executable ".py")
                           executable)))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (let ((applications (string-append #$output "/share/applications")))
                (mkdir-p applications)
                (call-with-output-file (string-append applications
                                                      "/toutenclic.desktop")
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
                     #$output)))))))))
    (native-inputs (list unzip python-setuptools))
    (inputs (list python-pyqt))
    (synopsis "School tools for physically disabled children")
    (description
     "ToutEnClic is intended to facilitate the schooling of physically
disabled children in ordinary schools.  It is both a multi-page virtual
exercise book and a kit including pencil, scissors, glue, ruler, compass,
protractor and square.  A virtual keyboard is also available if the child does
not have any other specialized device.")
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
      #:qtbase qtbase
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-initial-values
            ;; Remove useless "Check for updates" action from menu.
            (lambda _
              (substitute* "src/core/UBSettings.cpp"
                (("(appHideCheckForSoftwareUpdate = .*?)false(\\);)" _ beg end)
                 (string-append beg "true" end))))))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list alsa-lib
           coreutils-minimal            ;for patched 'env' shebang
           cups-minimal
           ffmpeg
           freetype
           lame
           libass
           libfdk
           libtheora
           libva
           libvorbis
           libvpx
           libx264
           openssl
           opus
           poppler
           qt5compat
           qtdeclarative
           qtmultimedia
           qtsvg
           qtwebchannel
           qtwebengine
           quazip
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
    (version "7.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (let ((directory "https://www.lalescu.ro/liviu/fet/download/")
                  (base (string-append "fet-" version ".tar.bz2")))
              (list (string-append directory base)
                    (string-append directory "old/" base))))
       (sha256
        (base32 "0vjjvr9vs3vxncrikchmk60qa99d5wyxja6b3p46vgfaziv1nj31"))))
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
    (license license:agpl3)))

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
         "0psbdzirazfnn02hp3gsx7xxss9f1brv4ywp6a15ihvggjki1rxb"))
       (patches
        (search-patches "libeb-gcc-14.patch"))))
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
           qtwayland-5
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
    (version "3.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exercism/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w1md548janc16svdqij6bya5r6rayl13760jmsx28ws8yv2wjqf"))
       (patches (search-patches "exercism-disable-self-update.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:install-source? #f
      #:import-path "github.com/exercism/cli/exercism"
      #:unpack-path "github.com/exercism/cli"
      ;; Step away from cli/exercism to test the whole project.
      #:test-subdirs #~(list "../../...")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-xdg-open
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* "browser/open.go"
                  (("xdg-open")
                   (string-append #$(this-package-input "xdg-utils")
                                  "/bin/xdg-open"))))))
          (add-after 'install 'install-completions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((exercism (string-append #$output "/bin/exercism"))
                     (bash (string-append
                            #$output
                            "/etc/bash_completion.d/exercism"))
                     (fish (string-append
                            #$output
                            "/share/fish/vendor_completions.d/exercism.fish"))
                     (zsh (string-append
                           #$output
                           "/share/zsh/site-functions/_exercism")))
                (mkdir-p (dirname bash))
                (with-output-to-file bash
                  (lambda ()
                    (invoke exercism "completion" "bash")))
                (mkdir-p (dirname fish))
                (with-output-to-file fish
                  (lambda ()
                    (invoke exercism "completion" "fish")))
                (mkdir-p (dirname zsh))
                (with-output-to-file zsh
                  (lambda ()
                    (invoke exercism "completion" "zsh")))))))))
    (native-inputs
     (list go-github-com-blang-semver
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-spf13-viper
           go-github-com-stretchr-testify
           go-golang-org-x-net
           go-golang-org-x-text))
    (inputs
     (list xdg-utils))
    (home-page "https://exercism.org/")
    (synopsis "Mentored learning for programming languages")
    (description
     "Commandline client for exercism.io, a free service providing mentored
learning for programming languages.")
    (license license:expat)))

(define-public exercism-cli
  (package/inherit exercism
    (name "exercism-cli")))

(define-public mazo
  (package
    (name "mazo")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/luis-felipe/mazo.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mwhd8y3xs57rbaarpi1ams5i0kndizdpns4ym7dmnnn7s9g1xq8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getcwd))
                (invoke "python3" "manage.py" "test"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (script (string-append bin "/mazo"))
                     (share (string-append #$output "/share"))
                     (help (string-append share "/help/C/mazo")))
                (mkdir-p help)
                (mkdir-p bin)
                (copy-file "mazo.py" script)
                (chmod script #o555)
                (install-file "icons/mazo.svg"
                              (string-append share
                                             "/icons/hicolor/scalable/apps"))
                (install-file "lugare.ulkeva.Mazo.desktop"
                              (string-append share "/applications"))
                (copy-recursively "help/C/mazo" help)
                (copy-recursively "mazo"
                                  (string-append (site-packages inputs outputs)
                                                 "/mazo"))))))))
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
    (description
     "Mazo is a learning application that helps you memorize simple concepts
using multimedia flash cards and spaced reviews.")
    (license license:public-domain)))

(define-public hardv
  (package
    (name "hardv")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dongyx/hardv")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l1wb7hlyldl5hq0z9a1a1rrgn33rjyqb08jph3q7mcn5p3qx40i"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "prefix=" #$output)
                                (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (list "hardv.man1" "learn.c")
                     (("/bin/sh")
                      (search-input-file inputs "/bin/sh")))))
               (delete 'configure)
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   ;; Tests can only be run after installation.
                   (when tests?
                     (setenv "PATH"
                             (string-append #$output "/bin:" (getenv "PATH")))
                     (invoke "make" "test")))))))
    (home-page "https://github.com/dongyx/hardv")
    (synopsis "Spaced repetition flash card program")
    (description "HardV is a powerful spaced repetition flash card program.
The key features are:

@itemize
@item HardV runs in the CLI mode by default, but you may configure it
to be a TUI program, or to view images in a GUI window.
@item HardV can open the editor, send the content you wrote to an online
judging system, and determine the next quiz time by the judging result.
@item It can be used to implement keyboard shortcut practice, cloze deletion,
text-to-speech review, typing in the answer, and more.
@item The format of input files are easy to be parsed by both human and other
Unix utilities like grep, sed, and awk.
@item Metadata like scheduled time is written back to input files; thus all
your data is in files created and managed by yourself.
@item HardV is a Unix filter in the default mode; that makes it easy to be
called by other programs.  For example, you could pipe HardV to a voice
synthesizer to make an audio quiz.
@end itemize")
    (license license:bsd-2)))

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
