;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2019, 2020, 2021 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019, 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Sarthak Shah <shahsarthakw@gmail.com>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
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

(define-module (gnu packages toys)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public cbonsai
  ;; XXX: The latest release (1.3.1) was placed on <2021-08-14> but the
  ;; project has more updates since that time, use the latest commit instead.
  (let ((commit "4682ec7ca7f74eca0b05b2fad8a8301d16e6978f")
        (revision "1"))
    (package
      (name "cbonsai")
      (version (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/jallbrit/cbonsai.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01slgw872nwpbaa8h2q5s7dfrq3xan0mh6wh8waz88xhy8vp7z1n"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; No test suite
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target))
                (string-append "PREFIX=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure) ; No ./configure script
            (add-after 'install 'install-doc
              (lambda _
                (let ((doc (format #f "~a/share/doc/~a-~a" #$output
                                   #$name #$version)))
                  (install-file "README.md" doc)))))))
      (native-inputs
       (list pkg-config scdoc))
      (inputs
       (list ncurses))
      (home-page "https://gitlab.com/jallbrit/cbonsai")
      (synopsis "Grow bonsai trees in a terminal")
      (description "Cbonsai is a bonsai tree generator using ASCII art.  It
creates, colors, and positions a bonsai tree, and is configurable.")
      (license license:gpl3+))))

(define-public cxxmatrix
  (let ((commit "c8d4ecfb8b6c22bb93f3e10a9d203209ba193591")
        (revision "0"))
    (package
      (name "cxxmatrix")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/akinomyoga/cxxmatrix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0shlh0kbx5169pk1i72ymyyfadywyzmw4f95hn0lm8irjynizzg5"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                ;no tests
             #:make-flags #~(list (string-append "PREFIX=" #$output))
             #:phases #~(modify-phases %standard-phases (delete 'configure))))
      (home-page "https://github.com/akinomyoga/cxxmatrix")
      (synopsis "Configurable @emph{The Matrix} digital rain effect and more")
      (description "This package displays the ``digital rain'' effect from the 1999
film @emph{The Matrix} in the terminal with authentic hankaku kana characters and
bloom as well as rotating and zooming Mandelbrot set and Conway's game of life
effects.  The presented title text and color are configurable.")
      (license license:expat))))

(define-public daikichi
  (package
    (name "daikichi")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/lilyp/daikichi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "139kjf14ayqrwzd0hzw2qhfp47ngyimibzgfyqy6i7y8lhwsjs1v"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'hard-code-test-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (list "test-dat.in" "test-strings.in")
                     (("(basename|cmp|diff|mktemp|rm|sed|seq)" cmd)
                      (search-input-file inputs
                                         (string-append "bin/" cmd)))))))))
    (inputs (list bash-minimal coreutils sed
                  diffutils
                  fmt gmp))
    (native-inputs (list pkg-config))
    (home-page "https://gitlab.com/lilyp/daikichi")
    (synopsis "Display random fortunes")
    (description "Daikichi is an alternative implementation of
@command{fortune}, which displays random quotes from a database.
This package provides just the utilities and no quotes.")
    (license license:gpl3+)
    (native-search-paths
     (list (search-path-specification
            (variable "DAIKICHI_FORTUNE_PATH")
            (files '("share/fortunes")))))))

(define-public filters
  (package
    (name "filters")
    (version "2.55")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.joeyh.name/filters")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gaigpda1w9wxfh8an3sam1hpacc1bhxl696w4yj0vzhc6izqvxs"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; kenny is under nonfree Artistic License (Perl) 1.0.
                   (delete-file "kenny")
                   (substitute* "Makefile"
                     (("kenny")
                      ""))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-build? #f           ; y.tab.h fails otherwise
      #:tests? #f                    ; no test suite
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'respect-prefix
            (lambda _
              (substitute* "Makefile"
                (("/usr/games")
                 "$(prefix)/bin/")
                (("/usr")
                 "$(prefix)")))))))
    (native-inputs
     (list bison flex))
    (inputs
     (list perl))
    (home-page "https://joeyh.name/code/filters/")
    (synopsis "Various amusing text filters")
    (description
     "The filters collection harks back to the late 1980s, when various text
filters were written to munge written language in amusing ways.  The earliest
and best known were legends such as the Swedish Chef filter and B1FF.

This package contains the following filter commands:
@enumerate
@item b1ff: a satire of a stereotypical Usenet newbie
@item censor: comply with the @acronym{CDA, Communications Decency Act}
@item chef: convert English to Mock Swedish
@item cockney: Cockney English
@item elee: k3wl hacker slang
@item fanboy: a stereotypical fan (supports custom fandoms)
@item fudd: Elmer Fudd
@item jethro: hillbilly text filter
@item jibberish: a random selection of these filters
@item jive: Jive English
@item ken: turn English into Cockney
@item kraut: a bad German accent
@item ky00te: a very cute accent
@item LOLCAT: as seen in Internet GIFs everywhere
@item nethackify: wiped-out text as found in nethack
@item newspeak: à la 1984
@item nyc: Brooklyn English
@item pirate: talk like a pirate
@item rasterman: straight from the keyboard of Carsten Haitzler
@item scottish: fake Scottish (Dwarven) accent
@item scramble: scramble the \"inner\" letters of each word
@item spammer: turn honest text into something liable to be flagged as spam
@item studly: studly caps.
@item uniencode: use glorious Unicode to the fullest possible extent
@item upside-down: flip the text upside down
@end enumerate

The GNU project hosts a similar collection of filters, the GNU talkfilters.")
    (license                      ; see debian/copyright
     (list license:gpl2+          ; most of the filters
           license:gpl2           ; rasterman, ky00te.dir/* nethackify, pirate
           license:gpl3+          ; scramble, scottish
           license:public-domain  ; jethro, kraut, ken, studly
           license:gpl1+          ; cockney, jive, nyc only say "gpl"
           license:expat))))     ; newspeak

(define-public fortunes-jkirchartz
  ;; No public release.
  ;; Note to updaters: Please ensure that new quotes do not bring harm
  ;; rather than fortune.
  (let ((commit "f3acb4e88830da7d2ef6ffe501a486a0beb4cea8")
        (revision "1"))
    (package
      (name "fortunes-jkirchartz")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JKirchartz/fortunes")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1l1idf1d5y7acq0znyz9yf7073i4k6wp5pdbn1vaq2np5crpl2r5"))
                (snippet
                 #~(for-each delete-file
                             ;; incompatible license
                             '("BibleAbridged")))))
      (build-system copy-build-system)
      (native-inputs (list daikichi gnu-make))
      (arguments
       (list #:install-plan
             #~`(("." "share/fortunes" #:include-regexp ("\\.dat$")))
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-source
                   (lambda* (#:key inputs native-inputs #:allow-other-keys)
                     (substitute* "showerthoughts"
                       (("&lt;") "<")
                       (("&gt;") ">")
                       (("&amp;") "&"))
                     (substitute* "Makefile"
                       (("strfile") "daikichi pack"))))
                 (add-before 'install 'build
                   (lambda _
                     (invoke "make")))
                 (add-after 'build 'check
                   (lambda* (#:key inputs tests? #:allow-other-keys)
                     (when tests?
                       (apply
                        invoke
                        (search-input-file inputs "libexec/daikichi/test-dat")
                        (find-files "." "\\.dat$"))))))))
      (home-page "https://github.com/JKirchartz/fortunes")
      (synopsis "Collection of fortunes")
      (description "This package contains a large collection of quotes to
display via @command{fortune}, drawn from sources all around the world.")
      (license license:unlicense))))

(define-public lolcat
  (package
    (name "lolcat")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaseg/lolcat")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1vq5y4wzjnj5r9jd085mifw84wz6hnp8p9gnd2d3x3jg9xwb0jmc"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                    ; no check target
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (replace 'install
            (lambda _
              (let ((dest (string-append #$output "/bin")))
                (mkdir-p dest)
                (install-file "lolcat" dest)
                (install-file "censor" dest)))))))
    (home-page "https://github.com/jaseg/lolcat")
    (synopsis "Rainbow coloring effect for text console display")
    (description "@command{lolcat} concatenates files and streams like
regular @command{cat}, but it also adds terminal escape codes between
characters and lines resulting in a rainbow effect.")
    (license license:wtfpl2)))

(define-public nyancat
  (package
    (name "nyancat")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klange/nyancat")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mg8nm5xzcq1xr8cvx24ym2vmafkw53rijllwcdm9miiz0p5ky9k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure script
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (man (string-append #$output "/share/man/man1")))
                (install-file "src/nyancat" bin)
                (install-file "nyancat.1" man)))))))
    (home-page "https://nyancat.dakko.us/")
    (synopsis "Nyan cat telnet server")
    (description
     "This is an animated, color, ANSI-text telnet server that renders a loop
of the Nyan Cat / Poptart Cat animation.")
    (license license:ncsa)))

(define-public oneko
  (package
    (name "oneko")
    (version "1.2.sakura.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.daidouji.com/oneko/distfiles/oneko-" version ".tar.gz"))
       (sha256
        (base32 "0bxjlbafn10sfi5d06420pg70rpvsiy5gdbm8kspd6qy4kqhabic"))
       (patches (search-patches "oneko-remove-nonfree-characters.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bitmaps with copyright issues.
        '(begin
           (for-each delete-file-recursively
                     (cons* "bitmaps/bsd" "bitmaps/sakura" "bitmaps/tomoyo"
                            "bitmasks/bsd" "bitmasks/sakura" "bitmasks/tomoyo"
                            (find-files "cursors" "(bsd|card|petal).*\\.xbm")))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (invoke "xmkmf")
              ;; Fix incorrectly generated compiler flags.
              (substitute* "Makefile"
                (("(CDEBUGFLAGS = ).*" _ front)
                 (string-append front "-O2\n")))))
          (replace 'install
            (lambda _
              (let* ((bin     (format #f "~a/bin" #$output))
                     (doc     (format #f "~a/share/doc/~a-~a"
                                      #$output #$name #$version))
                     (man     (format #f "~a/share/man" #$output))
                     (man6    (format #f "~a/man6" man))
                     (man6-ja (format #f "~a/ja/man6" man)))
                (install-file "oneko" bin)
                (mkdir-p man6)
                (mkdir-p man6-ja)
                (copy-file "oneko.man" (string-append man6 "/oneko.6"))
                (copy-file "oneko.man.jp" (string-append man6-ja "/oneko.6"))
                (for-each (lambda (file) (install-file file doc))
                          (find-files "." "README.*"))))))))
    (native-inputs
     (list imake))
    (inputs
     (list libx11 libxext))
    (home-page "http://www.daidouji.com/oneko/")
    (synopsis "Cute cat chasing your mouse pointer")
    (description
     "Oneko displays an animated cat or dog that chases the mouse
pointer---now an actual mouse or a bone---around the screen while you work.

It was written for the X Window system and does not work well on Wayland.")
    ;; See <https://directory.fsf.org/wiki/Oneko>.
    (license license:public-domain)))

(define-public python-terminaltexteffects
  (package
    (name "python-terminaltexteffects")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/ChrisBuilds/terminaltexteffects")
             ;; XXX: The project has a tendency to change tag style
             ;; e.g. v0.2.1, 0.9.0, release-0.11.0.
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lakq27bxf8wn99gch37p2mqnbbax54y22qrb1h4x04da006kjz2"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (home-page "https://github.com/ChrisBuilds/terminaltexteffects")
    (synopsis "Terminal visual effects engine and demo toy")
    (description
     "TerminalTextEffects (TTE) is a terminal visual effects engine.  It can
be installed as a system application to produce effects in your terminal, or
as a Python library to enable effects within your Python scripts/applications.
It also includes a growing library of built-in effects which showcase the
engine's features, including complex character movement via Paths, Waypoints,
and motion easing, with support for quadratic/cubic bezier curves, complex
animations via Scenes with symbol/color changes, layers, easing, and Path
synced progression, and variable stop/step color gradient generation.  Runs
inline, preserving terminal state and workflow.")
    (license license:expat)))

(define-public sl
  (package
    (name "sl")
    (version "5.05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eyJhb/sl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11a1rdgb8wagikhxgm81g80g5qsl59mv4qgsval3isykqh8729bj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)            ; no configure script
          (delete 'check)                ; no tests
          (replace 'install              ; no ‘make install’ target
            (lambda _
              (let* ((bin     (string-append #$output "/bin"))
                     (man     (string-append #$output "/share/man"))
                     (man1    (string-append #$output "/man1"))
                     (man1-ja (string-append man "/ja/man1")))
                (install-file "sl" bin)
                (install-file "sl.1" man1)
                (mkdir-p man1-ja)
                (copy-file "sl.1.ja" (string-append man1-ja "/sl.1"))))))))
    (inputs
     (list ncurses))
    (home-page "http://www.tkl.iis.u-tokyo.ac.jp/~toyoda/index_e.html")
    (synopsis "Joke command to correct typing \"sl\" by mistake")
    (description
     "@dfn{SL} (for Steam Locomotive) displays one of several animated trains
on the text terminal.  It serves no useful purpose but to discourage
mistakenly typing @command{sl} instead of @command{ls}.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public xfishtank
  (package
    (name "xfishtank")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.ratrabbit.nl/downloads/xfishtank/xfishtank-"
             version "~pre1.tar.gz"))
       ;; Version has ~pre1 in it.  Guix store does not allow tilde in file
       ;; names.  Save it in the Store using a hyphen.
       (file-name (string-append name "-" version "-pre1.tar.gz"))
       (sha256
        (base32 "16i9diawkmar6dhx5xn0mflr2h585gamab6137hvxzgaczx55lwp"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-install-path
            (lambda _
              ;; Install program to bin/ instead of games/.
              (substitute* "src/Makefile.in"
                (("(gamesdir = \\$\\(exec_prefix\\)/)games" _ prefix)
                 (string-append prefix "bin"))))))))
    (inputs
     (list gtk+ libx11 libxml2 libxpm libxt))
    (native-inputs
     (list pkg-config))
    (home-page
     "https://www.ratrabbit.nl/ratrabbit/software/xfishtank/index.html")
    (synopsis "Let fish swim over your desktop!")
    (description "Xfishtank is a vintage application that uses the X11
protocol.  It shows fishes swimming over the desktop.")
    (license (list license:expat license:gpl3+))))

(define-public xpenguins
  (package
    (name "xpenguins")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ratrabbit.nl/downloads/xpenguins/xpenguins-"
             version ".tar.gz"))
       (sha256
        (base32 "03qwc7gf21d2ixkrxxwwgayj6f5fv1kg4b7ggx90j5269il63adm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-install-path
            (lambda _
              ;; Install program to bin/ instead of games/.
              (substitute* "src/Makefile.in"
                (("(gamesdir = \\$\\(exec_prefix\\)/)games" _ prefix)
                 (string-append prefix "bin"))))))))
    (inputs
     (list gtk+ libx11 libxml2 libxpm libxt))
    (native-inputs
     (list pkg-config))
    (home-page
     "https://www.ratrabbit.nl/ratrabbit/software/xpenguins/index.html")
    (synopsis "Let penguins take over your desktop!")
    (description "Xpenguins is a vintage application showing penguins running,
flying and falling on the desktop, using windows as run paths.")
    (license license:gpl2+)))

(define-public xsnow
  (package
    (name "xsnow")
    (version "3.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.ratrabbit.nl/downloads/xsnow/xsnow-"
             version ".tar.gz"))
       (sha256
        (base32 "1wiwlqbc6lfcq69hln8mxsms327sjbdpv0mmkfi9j2xrcgmw41bs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-install-path
           (lambda _
             ;; Install program to bin instead of games.
             (substitute* "src/Makefile.in"
               (("(gamesdir = \\$\\(exec_prefix\\)/)games" _ prefix)
                (string-append prefix "bin")))
             #t)))))
    (inputs
     (list gsl libx11 libxpm libxt libxml2))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list gdk-pixbuf gtk+))
    (home-page "https://www.ratrabbit.nl/ratrabbit/xsnow/index.html")
    (synopsis "Let it snow on the desktop")
    (description "@code{Xsnow} animates snowfall and Santa with reindeer on
the desktop background.  Additional customizable effects include wind, stars
and various scenery elements.")
    (license license:gpl3+)))

;;;
;;; Avoid adding new packages to the end of this file.  To reduce the chances
;;; of a merge conflict, place them above by existing packages in alphabetical
;;; order.
;;;
