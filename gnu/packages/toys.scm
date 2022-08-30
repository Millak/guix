;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2019, 2020, 2021 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019, 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public daikichi
  (package
    (name "daikichi")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/lilyp/daikichi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y35f1qpxl743s0s83dg5ivkvprv19mqn0azm14k3y8pmp6cs52z"))))
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

(define-public fortunes-jkirchartz
  ;; No public release.
  ;; Note to updaters: Please ensure that new quotes do not bring harm
  ;; rather than fortune.
  (let ((commit "2e32ba0a57e3842dc06c8128d880ab4c8ec3aefc")
        (revision "0"))
    (package
      (name "fortunes-jkirchartz")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JKirchartz/fortunes")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ym4ldzww5yfd76q7zvhi491bqlykfjnc215bqx1cbj0c8ndb2l4"))
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
  (let ((commit "35dca3d0a381496d7195cd78f5b24aa7b62f2154")
        (revision "0"))
    (package
      (name "lolcat")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jaseg/lolcat")
               (commit commit)))
         (sha256
          (base32
           "0jjbkqcc2ikjxd1xgdyv4rb0vsw218181h89f2ywg29ffs3ypd8g"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:make-flags
         (list ,(string-append "CC=" (cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (dest (string-append out "/bin")))
                 (mkdir-p dest)
                 (install-file "lolcat" dest)
                 (install-file "censor" dest)
                 #t))))))
      (home-page "https://github.com/jaseg/lolcat")
      (synopsis "Rainbow coloring effect for text console display")
      (description "@command{lolcat} concatenates files and streams like
regular @command{cat}, but it also adds terminal escape codes between
characters and lines resulting in a rainbow effect.")
      (license license:wtfpl2))))

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
    (native-inputs
     (list imake))
    (inputs
     (list libx11 libxext))
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "xmkmf")
             ;; Fix incorrectly generated compiler flags.
             (substitute* "Makefile"
               (("(CDEBUGFLAGS = ).*" _ front) (string-append front "-O2\n")))))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (man (string-append out "/share/man"))
                    (man6 (string-append man "/man6"))
                    (man6-ja (string-append man "/ja/man6")))
               (install-file "oneko" bin)
               (mkdir-p man6)
               (mkdir-p man6-ja)
               (copy-file "oneko.man" (string-append man6 "/oneko.6"))
               (copy-file "oneko.man.jp" (string-append man6-ja "/oneko.6"))
               (for-each (lambda (file) (install-file file doc))
                         (find-files "." "README.*"))))))))
    (home-page "http://www.daidouji.com/oneko/")
    (synopsis "Cute cat chasing your mouse pointer")
    (description
     "Oneko displays an animated cat or dog that chases the mouse pointer---now
an actual mouse or a bone---around the screen while you work.

It was written for the X Window system and does not work well on Wayland.")
    (license license:public-domain))) ; see https://directory.fsf.org/wiki/Oneko

(define-public sl
  (package
    (name "sl")
    (version "5.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mtoyoda/sl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zrfd71zx2px2xpapg45s8xvi81xii63yl0h60q72j71zh4sif8b"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check)                ; no tests
         (replace 'install              ; no ‘make install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man"))
                    (man1 (string-append man "/man1"))
                    (man1-ja (string-append man "/ja/man1")))
               (install-file "sl" bin)
               (install-file "sl.1" man1)
               (mkdir-p man1-ja)
               (copy-file "sl.1.ja" (string-append man1-ja "/sl.1"))
               #t))))))
    (home-page "http://www.tkl.iis.u-tokyo.ac.jp/~toyoda/index_e.html")
    (synopsis "Joke command to correct typing \"sl\" by mistake")
    (description
     "@dfn{SL} (for Steam Locomotive) displays one of several animated trains
on the text terminal.  It serves no useful purpose but to discourage mistakenly
typing @command{sl} instead of @command{ls}.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

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
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'respect-prefix
           (lambda _
             (substitute* "Makefile"
               (("/usr/games")
                "$(prefix)/bin/")
               (("/usr")
                "$(prefix)"))
             #t)))
       #:tests? #f))                    ; no test suite
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

(define-public xsnow
  (package
    (name "xsnow")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.ratrabbit.nl/downloads/xsnow/xsnow-"
             version ".tar.gz"))
       (sha256
        (base32 "17pxc955jgkjan8ax0lw3b3sibw7aikc7p9qbxsp0w7g7jkxf666"))))
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
     (list gtk+ libx11 libxpm libxt libxml2))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.ratrabbit.nl/ratrabbit/xsnow/index.html")
    (synopsis "Let it snow on the desktop")
    (description "@code{Xsnow} animates snowfall and Santa with reindeer on
the desktop background.  Additional customizable effects include wind, stars
and various scenery elements.")
    (license license:gpl3+)))

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
         (base32
          "1mg8nm5xzcq1xr8cvx24ym2vmafkw53rijllwcdm9miiz0p5ky9k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '(,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "src/nyancat" bin)
               (install-file "nyancat.1" man))
             #t)))))
    (home-page "https://nyancat.dakko.us/")
    (synopsis "Nyan cat telnet server")
    (description
     "This is an animated, color, ANSI-text telnet server that renders a loop
of the Nyan Cat / Poptart Cat animation.")
    (license license:ncsa)))

(define-public cbonsai
  (package
    (name "cbonsai")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/jallbrit/cbonsai.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1krsrf7gilmpnba6hjgz8mk32vs55b4i1rxlp7ajrw0v487blljw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-"
                                        ,(package-version this-package))))
               (install-file "README.md" doc)))))))
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list ncurses))
    (home-page "https://gitlab.com/jallbrit/cbonsai")
    (synopsis "Grow bonsai trees in a terminal")
    (description "Cbonsai is a bonsai tree generator using ASCII art.  It
creates, colors, and positions a bonsai tree, and is configurable.")
    (license license:gpl3+)))
