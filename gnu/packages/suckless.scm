;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2015 Dmitry Bogatov <KAction@gnu.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
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

(define-module (gnu packages suckless)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libbsd))

(define-public blind
  (package
    (name "blind")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/blind-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1b36k8fg2gmabm69jckqja49i8y4rcbccgvv2wija15ciszrm1x9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags (list
                     "CC=gcc"
                     (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (synopsis "Command line video editing utilities")
    (home-page "http://tools.suckless.org/blind/")
    (description
     "Blind is a collection of command line video editing utilities.  It uses
a custom raw video format with a simple container.")
    (license license:isc)))

(define-public dwm
  (package
    (name "dwm")
    (version "6.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://dl.suckless.org/dwm/dwm-"
                                 version ".tar.gz"))
             (sha256
              (base32 "1zkmwb6df6m254shx06ly90c0q4jl70skk1pvkixpb7hcxhwbxn2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "FREETYPEINC="
                                         (assoc-ref %build-inputs "freetype")
                                         "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "Makefile" (("\\$\\{CC\\}") "gcc"))
             #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (zero?
               (system* "make" "install"
                        (string-append "DESTDIR=" out) "PREFIX=")))))
        (add-after 'build 'install-xsession
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Add a .desktop file to xsessions.
            (let* ((output (assoc-ref outputs "out"))
                   (xsessions (string-append output "/share/xsessions")))
              (mkdir-p xsessions)
              (with-output-to-file
                  (string-append xsessions "/dwm.desktop")
                (lambda _
                  (format #t
                    "[Desktop Entry]~@
                     Name=dwm~@
                     Comment=Dynamic Window Manager~@
                     Exec=~a/bin/dwm~@
                     TryExec=~@*~a/bin/dwm~@
                     Icon=~@
                     Type=Application~%"
                    output)))
              #t))))))
    (inputs
     `(("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)))
    (home-page "http://dwm.suckless.org/")
    (synopsis "Dynamic window manager")
    (description
     "dwm is a dynamic window manager for X.  It manages windows in tiled,
monocle and floating layouts.  All of the layouts can be applied dynamically,
optimising the environment for the application in use and the task performed.")
    (license license:x11)))

(define-public dmenu
  (package
    (name "dmenu")
    (version "4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/dmenu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cwnvamqqlgczvd5dv5rsgqbhv8kp0ddjnhmavb3q732i8028yja"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output)
                          (string-append "FREETYPEINC="
                                         (assoc-ref %build-inputs "freetype")
                                         "/include/freetype2"))
       #:phases
       (alist-delete 'configure %standard-phases)))
    (inputs
     `(("freetype" ,freetype)
       ("libxft" ,libxft)
       ("libx11" ,libx11)
       ("libxinerama" ,libxinerama)))
    (home-page "http://tools.suckless.org/dmenu/")
    (synopsis "Dynamic menu")
    (description
     "A dynamic menu for X, originally designed for dwm.  It manages large
numbers of user-defined menu items efficiently.")
    (license license:x11)))

(define-public spoon
  (package
    (name "spoon")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "10c5i7ykpy7inzzfiw1dh0srpkljycr3blxhvd8160wsvplbws48"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))))
    (inputs
     `(("libx11" ,libx11)
       ("libxkbfile" ,libxkbfile)
       ("alsa-lib" ,alsa-lib)
       ("libmpdclient" ,libmpdclient)))
    (home-page "https://git.2f30.org/spoon/")
    (synopsis "Set dwm status")
    (description
     "Spoon can be used to set the dwm status.")
    (license license:isc)))

(define-public slock
  (package
    (name "slock")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/slock-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0sif752303dg33f14k6pgwq2jp1hjyhqv6x4sy3sj281qvdljf5m"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases (alist-delete 'configure %standard-phases)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)))
    (home-page "http://tools.suckless.org/slock/")
    (synopsis "Simple X session lock")
    (description
     "Simple X session lock with trivial feedback on password entry.")
    (license license:x11)))

(define-public st
  (package
    (name "st")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.suckless.org/st/st-"
                           version ".tar.gz"))
       (sha256
        (base32
         "00309qiw20rc89696pk8bdr7ik4r1aarik7jxqk8k66cdj80v1zp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'inhibit-terminfo-install
                    (lambda _
                      (substitute* "Makefile"
                        (("\t@tic -s st.info") ""))
                      #t)))))
    (inputs
     `(("libx11" ,libx11)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://st.suckless.org/")
    (synopsis "Simple terminal emulator")
    (description
     "St implements a simple and lightweight terminal emulator.  It
implements 256 colors, most VT10X escape sequences, utf8, X11 copy/paste,
antialiased fonts (using fontconfig), fallback fonts, resizing, and line
drawing.")
    (license license:x11)))

(define-public surf
  (package
    (name "surf")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.suckless.org/surf/surf-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07cmajyafljigy10d21kkyvv5jf3hxkx06pz3rwwk3y3c9x4rvps"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Use the right file name for dmenu and xprop.
         (add-before 'build 'set-dmenu-and-xprop-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "config.def.h"
               (("dmenu") (string-append (assoc-ref inputs "dmenu") "/bin/dmenu"))
               (("xprop") (string-append (assoc-ref inputs "xprop") "/bin/xprop")))
             #t)))))
    (inputs
     `(("dmenu" ,dmenu)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)
       ("xprop" ,xprop)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://surf.suckless.org/")
    (synopsis "Simple web browser")
    (description
     "Surf is a simple web browser based on WebKit/GTK+.  It is able to
display websites and follow links.  It supports the XEmbed protocol which
makes it possible to embed it in another application.  Furthermore, one can
point surf to another URI by setting its XProperties.")
    (license license:x11)))

(define-public sent
  (package
    (name "sent")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/sent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0xhh752hwaa26k4q6wvrb9jnpbnylss2aw6z11j7l9rav7wn3fak"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ;no configuration
       #:tests? #f                      ;no test suite
       #:make-flags (let ((pkg-config (lambda (flag)
                                        (string-append
                                         "$(shell pkg-config " flag " "
                                         "xft fontconfig x11 libpng)"))))
                      (list
                       "CC=gcc"
                       (string-append "PREFIX=" %output)
                       (string-append "INCS=-I. " (pkg-config "--cflags"))
                       (string-append "LIBS=" (pkg-config "--libs") " -lm")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)))
    (synopsis "Plaintext presentation tool")
    (description "Sent uses plaintext files and PNG images to create slideshow
presentations.  Each paragraph represents a slide in the presentation.
Especially for presentations using the Takahashi method this is very nice and
allows you to write down the presentation for a quick lightning talk within a
few minutes.")
    (home-page "http://tools.suckless.org/sent")
    (license license:x11)))

(define-public xbattmon
  (package
    (name "xbattmon")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n2rrjq03pgqrdkl7cz5snsfdanf4s58w9h6dbvnl7p8bbd3j2kn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))))
    (inputs
     `(("libx11" ,libx11)))
    (home-page "https://git.2f30.org/xbattmon/")
    (synopsis "Simple battery monitor for X")
    (description
     "Xbattmon is a simple battery monitor for X.")
    (license license:isc)))

(define-public wificurse
  (package
    (name "wificurse")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "067ghr1xly5ca41kc83xila1p5hpq0bxfcmc8jvxi2ggm6wrhavn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list
                     (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (home-page "https://git.2f30.org/wificurse/")
    (synopsis "Wifi DoS attack tool")
    (description
     "Wificurses listens for beacons sent from wireless access points
in the range of your wireless station.  Once received the program
extracts the BSSID of the AP and transmits deauthentication packets
using the broadcast MAC address.  This results to the disconnection
of all clients connected to the AP at the time of the attack.  This
is essencially a WiFi DoS attack tool created for educational
purposes only.  It works only in Linux and requires wireless card
drivers capable of injecting packets in wireless networks.")
    (license license:gpl3+)))

(define-public skroll
  (package
    (name "skroll")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0km6bjfz4ssb1z0xwld6iiixnn7d255ax8yjs3zkdm42z8q9yl0f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (home-page "https://2f30.org/")
    (synopsis "Commandline utility which scrolls text")
    (description
     "Skroll is a small utility that you can use to make a text scroll.
Pipe text to it, and it will scroll a given number of letters from right to
left.")
    (license license:wtfpl2)))

(define-public sbm
  (package
    (name "sbm")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nks5mkh5wn30kyjzlkjlgi31bv1wq52kbp0r6nzbyfnvfdlywik"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (home-page "https://git.2f30.org/sbm/")
    (synopsis "Simple bandwidth monitor")
    (description
     "Sbm is a simple bandwidth monitor.")
    (license license:isc)))

(define-public prout
  (package
    (name "prout")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s6c3ygg1h1fyxkh8gd7nzjk6qhnwsb4535d2k780kxnwns5fzas"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (inputs
     `(("cups-minimal" ,cups-minimal)
       ("zlib" ,zlib)))
    (home-page "https://git.2f30.org/prout/")
    (synopsis "Smaller lp command")
    (description
     "Prout (PRint OUT) is a small utility one can use to send
documents to a printer.
It has no feature, and does nothing else.  Just set your default
printer in client.conf(5) and start printing.  No need for a local
cups server to be installed.")
    (license license:wtfpl2)))

(define-public noice
  (package
    (name "noice")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ldkbb71z6k4yzj4kpg3s94ijj1c1kx9dfcjz393py09scfyg5hr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure script
         (add-before 'build 'curses
           (lambda _
             (substitute* "Makefile"
               (("lcurses") "lncurses")))))))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://git.2f30.org/noice/")
    (synopsis "Small file browser")
    (description
     "Noice is a small curses-based file browser.")
    (license license:bsd-2)))

;;; We want some commits that are more recent than the latest release, 0.2
(define-public human
  (let ((commit "50c80e6ba12823184b6866e06b955dbd2ccdc5d7")
        (revision "1"))
    (package
      (name "human")
      (version (string-append "0.2-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.2f30.org/human.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "18xngm4h9vsyip52zwd79rrp1irzg6rs462lpbp61amf7hj955gn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (home-page "https://git.2f30.org/human/")
    (synopsis "Convert bytes to human readable formats")
    (description
     "Human is a small program which translate numbers into a
human readable format.  By default, it tries to detect the best
factorisation, but you can force its output.
You can adjust the number of decimals with the @code{SCALE}
environment variable.")
    (license license:wtfpl2))))

(define-public fortify-headers
  (package
    (name "fortify-headers")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cacdczpjb49c4i1168g541wnl3i3gbpv2m2wbnmw5wddlyhgkdg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (home-page "https://git.2f30.org/fortify-headers/")
    (synopsis "Standalone fortify-source implementation")
    (description
     "This is a standalone implementation of fortify source.  It provides
compile time buffer checks.  It is libc-agnostic and simply overlays the
system headers by using the @code{#include_next} extension found in GCC.  It was
initially intended to be used on musl-based Linux distributions.

@itemize
@item It is portable, works on *BSD, Linux, Solaris and possibly others.
@item It will only trap non-conformant programs.  This means that fortify
  level 2 is treated in the same way as level 1.
@item Avoids making function calls when undefined behaviour has already been
  invoked.  This is handled by using @code{__builtin_trap()}.
@item Support for out-of-bounds read interfaces, such as @code{send()},
  @code{write()}, @code{fwrite()}, etc.
@item No ABI is enforced.  All of the fortify check functions are inlined
  into the resulting binary.
@end itemize\n")
    (license license:isc)))

(define-public colors
  (package
    (name "colors")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.2f30.org/releases/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lckmqpgj89841splng0sszbls2ag71ggkgr1wsv9y3v6y87589z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (inputs
     `(("libpng" ,libpng)))
    (home-page "https://git.2f30.org/colors/")
    (synopsis "Extract colors from pictures")
    (description
     "Extract colors from PNG files.  It is similar to
strings(1) but for pictures.  For a given input file it outputs a
colormap to stdout.")
    (license license:isc)))

;; No new releases were made at github, this repository is more active than
;; the one at http://git.suckless.org/libutf/ and it is
;; done by the same developer.
(define-public libutf
  (let ((revision "1")
        (commit "ff4c60635e1f455b0a0b4200f8183fbd5a88225b"))
    (package
      (name "libutf")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cls/libutf")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1ih5vjavilzggyr1j1z6w1z12c2fs5fg77cfnv7ami5ivsy3kg3d"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; No tests
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)))) ; No configure script
      (inputs
       `(("gawk" ,gawk)))
      (home-page "https://github.com/cls/libutf")
      (synopsis "Plan 9 compatible UTF-8 library")
      (description
       "This is a C89 UTF-8 library, with an API compatible with that of
Plan 9's libutf, but with a number of improvements:

@itemize
@item Support for runes beyond the Basic Multilingual Plane.
@item utflen and utfnlen cannot overflow on 32- or 64-bit machines.
@item chartorune treats all invalid codepoints as though Runeerror.
@item fullrune, utfecpy, and utfnlen do not overestimate the length
of malformed runes.
@item An extra function, charntorune(p,s,n), equivalent to
fullrune(s,n) ? chartorune(p,s): 0.
@item Runeerror may be set to an alternative replacement value, such
as -1, to be used instead of U+FFFD.
@end itemize\n")
      (license license:expat))))

;; No release tarballs so far.
(define-public lchat
  (let ((revision "1")
        (commit "bbde23732f8c7769b982f0c1bda9b99fbf93f932"))
    (package
      (name "lchat")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/younix/lchat")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "00q3rc0aa5416jvjvrj71x1wnr0331kxhvjjs7pyxgnq4xf36k63"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; No tests
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; No configure script
           (add-before 'build 'libbsd
             (lambda _
               (substitute* "Makefile"
                 (("-lutf") "-lutf -lbsd"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "lchat" bin)
                 #t))))))
      (inputs
       `(("grep" ,grep)
         ("ncurses" ,ncurses)
         ("libutf" ,libutf)
         ("libbsd" ,libbsd)))
      (home-page "https://github.com/younix/lchat")
      (synopsis "Line chat is a frontend for the irc client ii from suckless")
      (description
       "Lchat (line chat) is the little and small brother of cii.
It is a front end for ii-like chat programs.  It uses tail(1) -f to get the
chat output in background.")
      (license license:isc))))
