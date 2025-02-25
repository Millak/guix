;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2021, 2023-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mckinley Olsen <mck.olsen@gmail.com>
;;; Copyright © 2016, 2017, 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 luhux <luhux@outlook.com>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021, 2022, 2024 Raphaël Mélotte <raphael.melotte@mind.be>
;;; Copyright © 2021 ikasero <ahmed@ikasero.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2022 Felipe Balbi <balbi@kernel.org>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022, 2023 jgart <jgart@dismail.de>
;;; Copyright © 2023 Aaron Covrig <aaron.covrig.us@ieee.org>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Suhail <suhail@bayesians.ca>
;;; Copyright © 2024 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Roman Scherer <roman@burningswell.com>
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

(define-module (gnu packages terminals)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public libptytty
  (package
    (name "libptytty")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yusiwen/libptytty")
             (commit "b9694ea18e0dbd78213f55233a430325c13ad63e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g8by1m6ya4r47p137mw4ddml40js0zh6mdb9n6ib49ayngv8ak3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (home-page "https://github.com/yusiwen/libptytty")
    (synopsis
     "Portable, secure PTY/TTY and @file{utmp}/@file{wtmp}/@file{lastlog} handling")
    (description
     "Libptytty is a small C/C++ library to manage pseudo-ttys in a uniform way,
created out of frustration over the many differences of PTY/TTY handling in
different operating systems.

In addition to mere PTY/TTY management, it supports updating the session
database at @file{utmp}, and @file{wtmp}/@file{lastlog} for login shells.

It also supports @code{fork}ing after start-up and dropping privileges in the
calling process.  This reduces the potential attack surface: if the calling
process were to be compromised by the user starting the program, there would be
less to gain, as only the helper process is running with privileges (e.g.,
@code{setuid}/@code{setgid}).")
    (license license:gpl2+)))

(define-public tilda
  (package
    (name "tilda")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lanoxx/tilda")
             (commit (string-append "tilda-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ad5jlyg9izm2rid115dv70af6j5i96p91i685c0h9vlrn5sviqs"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'make-po-writable
                    (lambda _
                      (for-each make-file-writable
                                (find-files "po" ".")) #t)))))
    (native-inputs (list autoconf-2.71 automake gettext-minimal pkg-config))
    (inputs (list libconfuse vte/gtk+-3))
    (synopsis "GTK+-based drop-down terminal")
    (description
     "Tilda is a terminal emulator similar to normal terminals like
gnome-terminal (GNOME) or Konsole (KDE), with the difference that it drops down
from the edge of a screen when a certain configurable hotkey is pressed.  This
is similar to the built-in consoles in some applications.  Tilda is highly
configurable through a graphical wizard.")
    (home-page "https://github.com/lanoxx/tilda")
    (license license:gpl2+)))

(define-public termite
  (package
    (name "termite")
    (version "16.6")
    (source
      (origin
        (method url-fetch)
        ;; XXX: The release includes a modified version of VTE.
        (uri (string-append
              "https://github.com/aperezdc/termite/releases/download/v"
              version "/termite-" version ".tar.xz"))
        (sha256
         (base32
          "1n8x84pkp7l9xl0sd07jbj5gjb574qm3w7656qlnzw8hf9kr69il"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dvte:_systemd=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-xdg-open
                 (lambda _
                   (substitute* "termite.cc"
                     (("xdg-open") (which "xdg-open")))))
               (replace 'install
                 (lambda _
                   (invoke "meson" "install" "--skip-subprojects" "vte"))))))
    (inputs
     (list gnutls gtk+ pcre2 xdg-utils))
    (native-inputs
     (list (list glib "bin") pkg-config))
    (home-page "https://github.com/aperezdc/termite/")
    (synopsis "Keyboard-centric, VTE-based terminal")
    (description "Termite is a minimal terminal emulator, with a slightly
modified version of VTE exposing the necessary functions for keyboard text
selection and URL hints.  It was designed for use with tiling window
managers.")
    (license license:lgpl2.0+)))

(define-public asciinema
  (package
    (name "asciinema")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asciinema/asciinema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qhf4sc5fl81rpq3rgzy7qcch620dh12scvsbdfczfbyjb10ps2i"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-python-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "tests/pty_test.py"
                     (("python3") (search-input-file inputs "/bin/python3"))))))))
    (native-inputs
     (list python-pytest ; For tests.
           python-setuptools python-wheel))
    (home-page "https://asciinema.org")
    (synopsis "Terminal session recorder")
    (description
     "Use asciinema to record and share your terminal sessions, the right way.
Forget screen recording apps and blurry video.  Enjoy a lightweight, purely
text-based approach to terminal recording.")
    (license license:gpl3)))

(define-public libtsm
  (let ((commit "f70e37982f382b03c6939dac3d5f814450bda253")
        (revision "1"))
    (package
      (name "libtsm")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         ;; The freedesktop repository is no longer maintained.
         (uri (git-reference
               (url (string-append "https://github.com/Aetf/" name))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mwn91i5h5d518i1s05y7hzv6bc13vzcvxszpfh77473iwg4wprx"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove a bundled copy of libxkbcommon's xkbcommon-keysyms.h.
             (delete-file-recursively "external/xkbcommon")
             #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_TESTING=ON")))
      (native-inputs
       (list check libxkbcommon ; for xkbcommon-keysyms.h
             pkg-config))
      (synopsis "Xterm state machine library")
      (description "TSM is a state machine for DEC VT100-VT520 compatible
terminal emulators.  It tries to support all common standards while keeping
compatibility to existing emulators like xterm, gnome-terminal, konsole, etc.")
      (home-page "https://www.freedesktop.org/wiki/Software/libtsm")
      ;; Hash table implementation is lgpl2.1+ licensed.
      ;; The wcwidth implementation in external/wcwidth.{h,c} uses a license
      ;; derived from ISC.
      ;; UCS-4 to UTF-8 encoding is copied from "terminology" which is released
      ;; under the bsd 2 license.
      (license (list license:expat license:lgpl2.1+ license:isc license:bsd-2)))))

(define-public kmscon
  (let ((commit "01dd0a231e2125a40ceba5f59fd945ff29bf2cdc")
        (revision "1"))
    (package
      (name "kmscon")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                ;; The freedesktop repository is no longer maintained.
                (uri (git-reference
                      (url (string-append "https://github.com/Aetf/" name))
                      (commit commit)))
                (sha256
                 (base32
                  "0q62kjsvy2iwy8adfiygx2bfwlh83rphgxbis95ycspqidg9py87"))
                (patches
                 (search-patches "kmscon-runtime-keymap-switch.patch"))
                (modules '((guix build utils)))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(;; The closure of MESA is huge so we'd rather avoid it.
         #:disallowed-references (,mesa)

         #:phases (modify-phases %standard-phases
                    ;; Use elogind instead of systemd.
                    (add-before 'configure 'remove-systemd
                      (lambda _
                        (substitute* "configure"
                          (("libsystemd-daemon libsystemd-login")
                           "libelogind"))
                        (substitute* "src/uterm_systemd.c"
                          (("#include <systemd/sd-login.h>")
                           "#include <elogind/sd-login.h>")
                          ;; We don't have this header.
                          (("#include <systemd/sd-daemon\\.h>")
                           "")
                          ;; Replace the call to 'sd_booted' by the truth value.
                          (("sd_booted\\(\\)")
                           "1")))))))
      (native-inputs
       (list pkg-config
             autoconf
             automake
             libtool
             libxslt ;to build the man page
             docbook-xsl))
      (inputs
       `(("libdrm" ,libdrm)
         ("libtsm" ,libtsm)
         ("libxkbcommon" ,libxkbcommon)
         ("logind" ,elogind)
         ;; MESA can be used for accelerated video output via OpenGLESv2, but
         ;; it's a big dependency that we'd rather avoid in the installation
         ;; image.
         ;; ("mesa" ,mesa)
         ("pango" ,pango)
         ("udev" ,eudev)))
      (synopsis "Linux KMS-based terminal emulator")
      (description "Kmscon is a terminal emulator based on Linux's @dfn{kernel
mode setting} (KMS).  It can replace the in-kernel virtual terminal (VT)
implementation with a user-space console.  Compared to the Linux console,
kmscon provides enhanced features including XKB-compatible internationalized
keyboard support, UTF-8 input/font support, hardware-accelerated rendering,
multi-seat support, a replacement for @command{mingetty}, and more.")
      (home-page "https://www.freedesktop.org/wiki/Software/kmscon")
      ;; Hash table implementation is lgpl2.1+ licensed.
      ;; The wcwidth implementation in external/wcwidth.{h,c} uses a license
      ;; derived from ISC.
      ;; UCS-4 to UTF-8 encoding is copied from "terminology" which is released
      ;; under the bsd 2 license.
      ;; Unifont-Font is from http://unifoundry.com/unifont.html and licensed
      ;; under the terms of the GNU GPL.
      (license (list license:expat license:lgpl2.1+ license:bsd-2
                     license:gpl2+))
      (supported-systems (filter (cut string-suffix? "-linux" <>)
                                 %supported-systems)))))

(define-public libtermkey
  (package
    (name "libtermkey")
    (version "0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.leonerd.org.uk/code/libtermkey/"
                                  "libtermkey-" version ".tar.gz"))
              (sha256
               (base32 "002606rrxh5f6l6jrikl0dyxsknscdamq10av21xm0xa98ybsib9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list
                     (string-append "CC=" ,(cc-for-target))
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'patch-failing-test
           ;; XXX This undoes an upstream change in version 0.22 which ‘ensures
           ;; that the hooked function can invent TI strings for new terminal
           ;; types’.  That fails in the build environment.  Why?
           (lambda _
             (substitute* "t/40ti-override.c"
               (("vt750") "vt100")))))
       #:test-target "test"))
    (inputs (list ncurses))
    (native-inputs (list libtool perl-test-harness pkg-config))
    (synopsis "Keyboard entry processing library for terminal-based programs")
    (description
     "Libtermkey handles all the necessary logic to recognise special keys, UTF-8
combining, and so on, with a simple interface.")
    (home-page "https://www.leonerd.org.uk/code/libtermkey")
    (license license:expat)))

(define-public mlterm
  (package
    (name "mlterm")
    (version "3.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mlterm/01release/mlterm-"
                           version "/mlterm-" version ".tar.gz"))
       (sha256
        (base32 "1nah3fn055njwpr2nfl8zkr5r02n89mxxdxgcjyk9q8x74hngdxm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:configure-flags
       (list "--disable-static"
             "--enable-optimize-redrawing"
             "--with-imagelib=gdk-pixbuf")))
    (native-inputs (list gettext-minimal pkg-config))
    (inputs
     (list cairo
           fontconfig
           freetype
           fribidi
           gdk-pixbuf
           gtk+
           libx11
           libxext
           libxft))
    (home-page "https://mlterm.sourceforge.net/")
    (synopsis "Multi-Lingual TERMinal emulator")
    (description
     "mlterm is a multi-lingual terminal emulator.  It supports various complex
character sets and encodings from around the world.  It can display double-width
(e.g.  East Asian) glyphs, combining characters used for, e.g., Thai and
Vietnamese, and bi-directional scripts like Arabic and Hebrew.")
    (license license:bsd-3)))

(define-public mtm
  (package
    (name "mtm")
    (version "1.2.1")
    (source
     (origin
       (uri (git-reference
             (url "https://github.com/deadpixi/mtm")
             (commit version)))
       (method git-fetch)
       (sha256
        (base32 "0gibrvah059z37jvn1qs4b6kvd4ivk2mfihmcpgx1vz6yg70zghv"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-before 'build 'fix-headers
           (lambda _
             (substitute* "config.def.h"
               (("ncursesw/curses.h") "curses.h"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               ;; install binary
               (mkdir-p (string-append out "bin/"))
               (install-file "mtm" (string-append out "/bin"))
               ;; install manpage
               (mkdir-p (string-append out "share/man/man1"))
               (install-file "mtm.1" (string-append out "/share/man/man1"))
               ;; install terminfo
               (mkdir-p (string-append out "share/terminfo"))
               (invoke (string-append (assoc-ref inputs "ncurses") "/bin/tic")
                       "-x" "-s" "-o"
                       (string-append
                        out "/share/terminfo")
                       "mtm.ti")))))))
    (inputs
     (list ncurses))
    ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
    ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
    (native-search-paths
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (home-page "https://github.com/deadpixi/mtm")
    (synopsis "Micro Terminal Multiplexer")
    (description
     "This package provides multiplexer for the terminal focused on simplicity,
compatibility, size and stability.")
    (license (list license:gpl3+
                   license:bsd-3))))    ;vtparser.c

(define-public picocom
  (package
    (name "picocom")
    (version "3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npat-efault/picocom")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vvjydqf0ax47nvdyyl67jafw5b3sfsav00xid6qpgia1gs2r72n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "picocom" bin)
               (install-file "picocom.1" man)))))))
    (home-page "https://github.com/npat-efault/picocom")
    (synopsis "Minimal dumb-terminal emulator")
    (description
     "Picocom is a minimal dumb-terminal emulation program.  It was designed to
serve as a simple and manual modem configuration, testing, and debugging tool.
It also serves well as a low-tech serial communications program to allow access
to all types of devices that provide serial consoles.")
    (license license:gpl2+)))

(define-public beep
  (package
    (name "beep")
    (version "1.4.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; The original beep 1.3 at <http://www.johnath.com/beep> has been
             ;; unmaintained for some time, and vulnerable to at least two CVEs:
             ;; https://github.com/johnath/beep/issues/11#issuecomment-454056858
             ;; Use this maintained fork instead.
             (url "https://github.com/spkr-beep/beep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dgrb5yg4ys1fa4hs95iz3m2yhryfzzw0j6g6yf6vhbys4ihcf40"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "prefix=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (add-before 'check 'patch-tests
                 (lambda _
                   (substitute* "GNUmakefile"
                     (("/bin/bash")
                      (which "bash"))
                     ;; XXX In the build environment, $(PWD) is the *parent* directory
                     ;; /tmp/guix-build-beep-x.y.drv-0!  A pure guix shell works fine.
                     (("\\$\\(PWD\\)" pwd)
                      (string-append pwd "/source")))
                   (substitute* (find-files "tests" "\\.expected")
                     ;; The build environment lacks /dev/{console,tty*}.
                     ;; In fact, even nckx's regular Guix System lacks ttyS1…
                     ((": Permission denied")
                      ": No such file or directory"))))
               (add-before 'install 'install-rules
                 (lambda _
                   (mkdir-p (string-append #$output "/etc/udev/rules.d"))
                   (with-output-to-file
                       (string-append #$output
                                      "/etc/udev/rules.d/70-pcspkr-beep.rules")
                     (lambda _
                       (display (string-append "\
ACTION==\"add\", SUBSYSTEM==\"input\", ATTRS{name}==\"PC Speaker\", "
                                               "ENV{DEVNAME}!=\"\", "
                                               "TAG+=\"uaccess\"")))))))))
    (synopsis "Linux command-line utility to control the PC speaker")
    (description "beep allows the user to control the PC speaker with precision,
allowing different sounds to indicate different events.  While it can be run
quite happily on the command line, its intended place of residence is within
scripts, notifying the user when something interesting occurs.  Of course, it
has no notion of what's interesting, but it's very good at that notifying part.")
    (home-page "https://github.com/spkr-beep/beep")
    (license license:gpl2+)))

(define-public unibilium
  (package
    (name "unibilium")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mauke/unibilium")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wa9a32wzqnxqh1jh554afj13dzjr6mw2wzqzw8d08nza9pg2ra2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       ;; FIXME: tests require "prove"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list libtool perl))
    (home-page "https://github.com/mauke/unibilium")
    (synopsis "Terminfo parsing library")
    (description "Unibilium is a basic C terminfo library.  It doesn't depend
on curses or any other library.  It also doesn't use global variables, so it
should be thread-safe.")
    (license license:lgpl3+)))

(define-public libvterm
  (package
    (name "libvterm")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/libvterm/trunk/v"
             (version-major+minor version)
             "/+download/libvterm-" version ".tar.gz"))
       (sha256
        (base32 "1q16fbznm54p24hqvw8c9v3347apk86ybsxyghsbsa11vm1ny589"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list
              ;; FIXME: cross build fails.
              ;; ld: src/.libs/encoding.o: error adding symbols: file in wrong format
              ;; collect2: error: ld returned 1 exit status
              (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
           #:test-target "test"
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (native-inputs
     (list libtool perl))
    (home-page "https://www.leonerd.org.uk/code/libvterm/")
    (synopsis "VT220/xterm/ECMA-48 terminal emulator library")
    (description "Libvterm is an abstract C99 library which implements a VT220
or xterm-like terminal emulator.  It doesn't use any particular graphics
toolkit or output system, instead it invokes callback function pointers that
its embedding program should provide it to draw on its behalf.  It avoids
calling @code{malloc} during normal running state, allowing it to be used in
embedded kernel situations.")
    (license license:expat)))

(define-public cool-retro-term
    (package
      (name "cool-retro-term")
      (version "1.2.0")
      (source (origin
                (method git-fetch)
                (file-name (string-append name "-" version "-checkout"))
                (uri (git-reference
                      (url (string-append "https://github.com/Swordfish90/" name))
                      (commit version)
                      (recursive? #t)))
                (sha256
                 (base32 "02mj70gcpx9fvrhsy6iqwp399dya9iyakx940b6ws952d23xn337"))
                (modules '((guix build utils)
                           (srfi srfi-1)
                           (srfi srfi-26)
                           (ice-9 rdelim)
                           (ice-9 regex)))
                (patches (search-patches "cool-retro-term-wctype.patch"))
                (snippet
                 '(let* ((fonts '(;"1971-ibm-3278"     ; BSD 3-clause
                                  "1977-apple2"        ; Non-Free
                                  "1977-commodore-pet" ; Non-Free
                                  "1979-atari-400-800" ; Non-Free
                                  ;"1981-ibm-pc        ; CC-SA 4.0
                                  "1982-commodore64")) ; Non-Free
                                  ;"1985-ibm-pc-vga"   ; CC-SA 4.0
                                  ;"modern-fixedsys-excelsior" ; Redistributable
                                  ;"modern-hermit"     ; SIL
                                  ;"modern-inconsolata"; SIL
                                  ;"modern-pro-font-win-tweaked" ; X11
                                  ;"modern-proggy-tiny"; X11
                                  ;"modern-terminus"   ; SIL
                         (name-rx (make-regexp " *name: *\"([^\"]*)\""))
                         (source-rx (make-regexp " *source: \"fonts/([^/]*)[^\"]*\""))
                         (fontname-rx (make-regexp "\"fontName\":\"([^\"]*).*"))
                         (names
                          ;; Gather font names from all Fonts*.qml files.
                          ;; These will be used to remove items from the
                          ;; default profiles.
                          (fold
                           (lambda (font-file names)
                             (call-with-input-file font-file
                               (lambda (port)
                                 (let loop ((name #f) (names names))
                                   (let ((line (read-line port)))
                                     (cond
                                      ((eof-object? line) (pk 'names names))
                                      ((regexp-exec name-rx line)
                                       => (lambda (m)
                                            (loop (match:substring m 1) names)))
                                      ((regexp-exec source-rx line)
                                       => (lambda (m)
                                            (let ((font (match:substring m 1)))
                                              (if (member font fonts)
                                                  (loop #f (lset-adjoin string=?
                                                                        names name))
                                                  (loop #f names)))))
                                      (else (loop name names))))))))
                           '() (find-files "app/qml" "Font.*\\.qml"))))
                    ;; Remove the font files themselves
                    (for-each (lambda (font)
                                (delete-file-recursively
                                 (string-append "app/qml/fonts/" font)))
                              fonts)
                    ;; Remove mention of those fonts in the source
                    (substitute* "app/qml/resources.qrc"
                      (((string-append " *<file>fonts/("
                                       (string-join fonts "|")
                                       ").*"))
                       ""))
                    (for-each
                     (lambda (file)
                       (let ((start-rx (make-regexp " *ListElement *\\{"))
                             (end-rx   (make-regexp " *\\}")))
                        (with-atomic-file-replacement file
                          (lambda (in out)
                            (let loop ((line-buffer '())
                                       (hold? #f)
                                       (discard? #f))
                              (let ((line (read-line in 'concat)))
                                (cond
                                 ((eof-object? line) #t) ;done
                                 ((regexp-exec start-rx line)
                                  (loop (cons line line-buffer) #t #f))
                                 ((or (regexp-exec source-rx line)
                                      (regexp-exec fontname-rx line))
                                  => (lambda (m)
                                       (let ((font-or-name (match:substring m 1)))
                                         (if (or (member font-or-name fonts)
                                                 (member font-or-name names))
                                             (loop '() #f #t)
                                             (loop (cons line line-buffer)
                                                   hold? #f)))))
                                 ((regexp-exec end-rx line)
                                  (unless discard?
                                          (for-each (cut display <> out)
                                                    (reverse line-buffer))
                                          (display line out))
                                  (loop '() #f #f))
                                 (hold? (loop (cons line line-buffer)
                                              hold? discard?))
                                 (discard? (loop line-buffer #f #t))
                                 (else (display line out)
                                       (loop '() #f #f)))))))))
                     '("app/qml/FontPixels.qml"
                       "app/qml/FontScanlines.qml"
                       "app/qml/Fonts.qml"
                       "app/qml/ApplicationSettings.qml"))
                    ;; Final substitution for default scanline and pixel fonts
                    (substitute* "app/qml/ApplicationSettings.qml"
                      (("COMMODORE_PET") "PROGGY_TINY"))))))
      (build-system gnu-build-system)
      (inputs
       (list qtbase-5 qtdeclarative-5 qtgraphicaleffects
             qtquickcontrols-5 qtquickcontrols2-5 bash-minimal))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* '("qmltermwidget/qmltermwidget.pro")
                   (("INSTALL_DIR = \\$\\$\\[QT_INSTALL_QML\\]")
                    (string-append "INSTALL_DIR = " out "/lib/qt5/qml")))
                 (substitute* '("cool-retro-term.pro" "app/app.pro")
                   (("/usr") out))
                 (invoke "qmake"))))
           (add-after 'install 'wrap-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (qml "/lib/qt5/qml"))
                 (wrap-program (string-append out "/bin/cool-retro-term")
                   `("QML2_IMPORT_PATH" ":" prefix
                     (,(string-append out qml)
                      ,@(map (lambda (i)
                               (string-append (assoc-ref inputs i) qml))
                             '("qtdeclarative"
                               "qtgraphicaleffects"
                               "qtquickcontrols"
                               "qtquickcontrols2"))))))))
           (add-after 'install 'add-alternate-name
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (symlink (string-append bin "/cool-retro-term")
                          (string-append bin "/crt")))))
           (add-after 'install 'install-man
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((mandir (string-append (assoc-ref outputs "out")
                                            "/share/man/man1")))
                 (install-file "packaging/debian/cool-retro-term.1" mandir)))))))
      (synopsis "Terminal emulator")
      (description
       "Cool-retro-term (crt) is a terminal emulator which mimics the look and
feel of the old cathode ray tube (CRT) screens.  It has been designed to be
eye-candy, customizable, and reasonably lightweight.")
      (home-page "https://github.com/Swordfish90/cool-retro-term")
      (license (list
                license:gpl2+           ; qmltermwidget
                license:gpl3+           ; cool-retro-term
                ;; Fonts
                license:silofl1.1
                license:x11
                license:bsd-3))))

(define-public foot
  (package
    (name "foot")
    (version "1.22.3")
    (home-page "https://codeberg.org/dnkl/foot")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l5liw4dgv7hxdimyk5qycmkfjgimdrx51rjvdizpcfmdlkvg518"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; Using a "release" build is recommended both for performance, and
      ;; also to address a GCC 10 issue when doing PGO builds.
      #:build-type "release"
      ;; Enable LTO as recommended by INSTALL.md.
      ;; when cross-compilation, enable lto will fail.
      #:configure-flags (if (%current-target-system)
                            #~'()
                            #~'("-Db_lto=true"))))
    (native-inputs (append
                    (if (%current-target-system)
                        (list wayland pkg-config-for-build)
                        '())
                    (list ncurses ;for 'tic'
                          pkg-config scdoc wayland-protocols-next)))
    (native-search-paths
     ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
     ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (inputs (list fcft libxkbcommon-1.8 wayland wayland-protocols-next))
    (synopsis "Wayland-native terminal emulator")
    (description
     "@command{foot} is a terminal emulator for systems using the Wayland
display server.  It is designed to be fast, lightweight, and independent of
desktop environments.  It can be used as a standalone terminal and also has
a server/client mode.")
    (license license:expat)))

(define-public havoc
  (package
    (name "havoc")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ii8/havoc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "089maf2xgh9halrccdj6p00l4q573x4f6a29655xb9h3a815s9k0"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (add-before 'build 'set-CC
            (lambda _
              (setenv "CC" #$(cc-for-target)))))))
    (native-inputs
     (list pkg-config wayland-protocols))
    (inputs
     (list libxkbcommon wayland))
    (home-page "https://github.com/ii8/havoc")
    (synopsis "Minimal terminal emulator for Wayland")
    (description
     "Havoc is a minimal terminal emulator for Wayland.")
    (license license:expat)))

(define-public sakura
  (package
    (name "sakura")
    (version "3.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/sakura/trunk/"
                                  version "/+download/sakura-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1d8n32xnj21q2xx13xs2r9cfjaq31mxiyhx6d57x9cwnhwb11xn3"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check phase
    (native-inputs
     (list gettext-minimal perl ; for pod2man
           pkg-config))
    (inputs
     (list libxft vte/gtk+-3))
    (home-page "https://launchpad.net/sakura")
    (synopsis "Simple but powerful libvte-based terminal emulator")
    (description "@code{Sakura} is a terminal emulator based on GTK+ and VTE.
It's a terminal emulator with few dependencies, so you don't need a full GNOME
desktop installed to have a decent terminal emulator.")
    (license license:gpl2)))

(define-public xiate
  (let ((commit "ae3cf30b345c64f097a747ac848e23ef5bae8b57")
        (revision "0"))
    (package
      (name "xiate")
      (version (git-version "22.12" revision commit))
      (source (origin
                (method git-fetch)
                (file-name (git-file-name name version))
                (uri (git-reference
                      (url "https://www.uninformativ.de/git/xiate.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0bc205b1gs1jvp1a2cr814l32hmlm0sgv1drfw7ykbavslfpmg2d"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f ;no tests
             #:make-flags #~(list (string-append "CC="
                                                 #$(cc-for-target))
                                  (string-append "prefix="
                                                 #$output))
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure))))
      (inputs (list gtk+ glib vte/gtk+-3))
      (native-inputs (list pkg-config))
      (synopsis "Minimalist terminal emulator based on GTK+")
      (description
       "Xiate is a terminal emulator which tries to keep a balance
between features and simplicity.  This is achieved by using VTE as a powerful
backend, while UI, configuration, and code try to remain much more
minimalistic.")
      (home-page "https://www.uninformativ.de/git/xiate/file/README.html")
      (license license:expat))))

(define-public go-github-com-junegunn-fzf
  (package
    (name "go-github-com-junegunn-fzf")
    (version "0.60.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/junegunn/fzf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c18h9326i8g9ksbfrpzrxpz8xlym2a35fpjsi7dn1dv6rr3jayn"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/junegunn/fzf"))
    (inputs
     (list go-github-com-charlievieth-fastwalk
           go-github-com-gdamore-tcell-v2
           go-github-com-junegunn-go-shellwords
           go-github-com-mattn-go-isatty
           go-github-com-rivo-uniseg
           go-golang-org-x-sys
           go-golang-org-x-term))
    (home-page "https://github.com/junegunn/fzf")
    (synopsis "Command-line fuzzy-finder")
    (description "This package provides an interactive command-line filter
usable with any list--including files, command history, processes and more.")
    (license license:expat)))

(define-public fzf
  (package
    (inherit go-github-com-junegunn-fzf)
    (name "fzf")
    (arguments
     (ensure-keyword-arguments
      (package-arguments go-github-com-junegunn-fzf)
      `(#:install-source? #f
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'copy-binaries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (with-directory-excursion "src/github.com/junegunn/fzf"
                  (install-file "bin/fzf-tmux"
                                (string-append out "/bin"))))))
          (add-after 'copy-binaries 'wrap-programs
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (findutils (assoc-ref inputs "findutils"))
                     (ncurses (assoc-ref inputs "ncurses")))
                (wrap-program (string-append bin "/fzf")
                  `("PATH" ":" prefix (,(string-append findutils "/bin"))))
                (wrap-program (string-append bin "/fzf-tmux")
                  `("PATH" ":" prefix (,(string-append ncurses "/bin")))))))
          (add-after 'install 'install-completions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash-completion (string-append out "/etc/bash_completion.d"))
                     (fish-functions
                       (string-append out "/share/fish/vendor_functions.d"))
                     (zsh-completion (string-append out "/share/zsh/site-functions")))
                (with-directory-excursion "src/github.com/junegunn/fzf"
                  (mkdir-p bash-completion)
                  (copy-file "shell/completion.bash"
                             (string-append bash-completion "/fzf"))
                  (mkdir-p fish-functions)
                  (copy-file "shell/key-bindings.fish"
                             (string-append fish-functions "/fzf_key_bindings.fish"))
                  (mkdir-p zsh-completion)
                  (copy-file "shell/completion.zsh"
                             (string-append zsh-completion "/_fzf"))))))))))
    (inputs
     `(,@(package-inputs go-github-com-junegunn-fzf)
       ("bash" ,bash-minimal) ; for wrap-program
       ("findutils" ,findutils)
       ("ncurses" ,ncurses)))))

(define-public python-pyte
  (package
    (name "python-pyte")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyte" version))
       (sha256
        (base32
         "1c4pn2qijk6q8q25klfq365gbvlkrh8c0lz5lrr7b7kmh6vx3gxr"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-test
           ;; TODO: Reenable when the `captured` files required by this test
           ;; are included in the archive.
           (lambda _
             (delete-file "tests/test_input_output.py")
             #t)))))
    (propagated-inputs
     (list python-wcwidth))
    (native-inputs
     (list python-pytest-runner python-pytest))
    (home-page "https://pyte.readthedocs.io/")
    (synopsis "Simple VTXXX-compatible terminal emulator")
    (description "@code{pyte} is an in-memory VTxxx-compatible terminal
emulator.  @var{VTxxx} stands for a series of video terminals, developed by
DEC between 1970 and 1995.  The first and probably most famous one was the
VT100 terminal, which is now a de-facto standard for all virtual terminal
emulators.

pyte is a fork of vt102, which was an incomplete pure Python implementation
of VT100 terminal.")
    (license license:lgpl3+)))

(define-public python-blessings
  (package
    (name "python-blessings")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blessings" version))
       (sha256
        (base32
         "0z8mgkbmisxs10rz88qg46l1c9a8n08k8cy2iassal2zh16qbrcq"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Test suite is unable to detect TTY conditions.
     `(#:tests? #f))
    (native-inputs
     (list python-nose python-six))
    (home-page "https://github.com/erikrose/blessings")
    (synopsis "Python module to manage terminal color, styling, and
positioning")
    (description "Blessings is a pythonic API to manipulate terminal color,
styling, and positioning.  It provides similar features to curses but avoids
some of curses’s limitations: it does not require clearing the whole screen
for little changes, provides a scroll-back buffer after the program exits, and
avoids styling altogether when the output is redirected to something other
than a terminal.")
    (license license:expat)))

(define-public python-curtsies
  (package
    (name "python-curtsies")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "curtsies" version))
       (sha256
        (base32 "03kn093lr84qg8fmqrn1jb0zak6a1ir9q106lm8jijfpbchk7gkf"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pyte
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-blessed
           python-cwcwidth))
    (home-page "https://github.com/bpython/curtsies")
    (synopsis "Library for curses-like terminal interaction with colored strings")
    (description
     "Curtsies is a Python library for interacting with the terminal.  It
features string-like objects which carry formatting information, per-line
fullscreen terminal rendering, and keyboard input event reporting.")
    (license license:expat)))

(define-public python-halo
  (package
    (name "python-halo")
    (version "0.0.31")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "halo" version))
              (sha256
               (base32
                "1mn97h370ggbc9vi6x8r6akd5q8i512y6kid2nvm67g93r9a6rvv"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-colorama python-log-symbols python-six
                             python-spinners python-termcolor))
    (native-inputs
     (list python-coverage
           python-nose
           python-pylint
           python-setuptools
           python-tox
           python-twine
           python-wheel))
    (home-page "https://github.com/manrajgrover/halo")
    (synopsis "Python library to display graphical spinners in the terminal")
    (description "Halo is a Python library to display graphical spinners in
the terminal.  It also supports IPython/Jupyter.")
    (license license:expat)))

(define-public python-log-symbols
  (package
    (name "python-log-symbols")
    (version "0.0.14")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "log_symbols" version))
              (sha256
               (base32
                "0mh5d0igw33libfmbsr1ri1p1y644p36nwaa2w6kzrd8w5pvq2yg"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   (substitute* "requirements-dev.txt"
                     (("(.*)==(.*)$" _ dep ver)
                      (string-append dep ">=" ver))))))))
    (native-inputs
     (list python-coverage
           python-nose
           python-pylint
           python-setuptools
           python-tox
           python-wheel))
    (propagated-inputs (list python-colorama))
    (home-page "https://github.com/manrajgrover/py-log-symbols")
    (synopsis "Python library with graphical symbols for logging on the terminal")
    (description "This package provides a Python library with graphical symbols
that can be displayed on the terminal, with color if possible, for logging
purposes.")
    (license license:expat)))

(define-public python-spinners
  (package
    (name "python-spinners")
    (version "0.0.24")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spinners" version))
              (sha256
               (base32
                "0zz2z6dpdjdq5z8m8w8dfi8by0ih1zrdq0caxm1anwhxg2saxdhy"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   (substitute* "requirements-dev.txt"
                     (("(.*)==(.*)$" _ dep ver)
                      (string-append dep ">=" ver))))))))
    (native-inputs
     (list python-coverage
           python-nose
           python-pylint
           python-setuptools
           python-tox
           python-wheel))
    (home-page "https://github.com/manrajgrover/py-spinners")
    (synopsis "Python library with graphical spinners for the terminal")
    (description "Spinners is a Python library that contains graphical spinners
that can be displayed terminal.")
    (license license:expat)))

(define-public tmate
  (package
    (name "tmate")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tmate-io/tmate")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x5c31yq7ansmiy20a0qf59wagba9v3pq97mlkxrqxn4n1gcc6vi"))))
    (build-system gnu-build-system)
    (inputs (list libevent libssh msgpack-3 ncurses))
    (native-inputs (list autoconf automake pkg-config))
    (home-page "https://tmate.io/")
    (synopsis "Terminal sharing application")
    (description "tmate is a terminal sharing application that allows you to
share your terminal with other users over the Internet.  tmate is a fork of
tmux.")
    (license license:isc)))

(define-public kitty
  (package
    (name "kitty")
    (version "0.21.2")
    (home-page "https://sw.kovidgoyal.net/kitty/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kovidgoyal/kitty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y0mg8rr18mn0wzym7v48x6kl0ixd5q387kr5jhbdln55ph2jk9d"))
       (patches (search-patches "kitty-fix-wayland-protocols.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; patch needed as sphinx-build is used as a python script
           ;; whereas the guix package uses a bash script launching the
           ;; python script
           (substitute* "docs/conf.py"
             (("(from kitty.constants import str_version)" kitty-imp)
              (string-append "sys.path.append(\"..\")\n" kitty-imp)))
           (substitute* "docs/Makefile"
             (("^SPHINXBUILD[[:space:]]+= (python3.*)$")
              "SPHINXBUILD = sphinx-build\n"))
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list dbus
           mesa
           libxcursor
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           ncurses ;; for tic command
           pkg-config
           python-sphinx
           wayland-protocols))
    (inputs
     (list fontconfig
           freetype
           harfbuzz
           lcms
           libcanberra
           libpng
           python-pygments
           python-wrapper
           wayland
           zlib))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)   ;no configure script
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Don't fail on deprecation warnings from GCC
              (setenv "CFLAGS" "-Wno-error=deprecated-declarations")
              ;; The "kitty" sub-directory must be writable prior to
              ;; configuration (e.g., un-setting updates).
              (for-each make-file-writable (find-files "kitty"))
              (invoke "python3" "setup.py" "linux-package"
                      ;; Do not phone home.
                      "--update-check-interval=0"
                      ;; Wayland backend requires EGL, which isn't
                      ;; found out-of-the-box for some reason.
                      (string-append "--egl-library="
                                     (search-input-file inputs "/lib/libEGL.so.1")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Fix "cannot find kitty executable" error when running
                ;; tests.
                (setenv "PATH" (string-append "linux-package/bin:"
                                              (getenv "PATH")))
                ;; Don't fail on deprecation warnings from Python
                (substitute* "test.py"
                  (("'error'") "'ignore'"))
                ;; Fails: No writable cache directories
                (substitute* "kitty_tests/fonts.py"
                  (("    def test_box_drawing")
                   (string-append
                    "    @unittest.skip('No writable cache directories')\n"
                    "    def test_box_drawing")))
                ;; Fails: Permission denied
                (substitute* "kitty_tests/parser.py"
                  (("import time")
                   "import time\nimport unittest\n")
                  (("    def test_graphics_command")
                   (string-append
                    "    @unittest.skip('Permission denied')\n"
                    "    def test_graphics_command")))
                ;; TypeError: expected bytes, str found
                (substitute* "kitty_tests/tui.py"
                  (("from . import BaseTest")
                   "from . import BaseTest\nimport unittest\n")
                  (("    def test_multiprocessing_spawn")
                   (string-append
                    "    @unittest.skip('TypeError: expected bytes, str found')\n"
                    "    def test_multiprocessing_spawn")))
                (invoke "python3" "test.py"))))
          (add-before 'install 'rm-pycache
            ;; created python cache __pycache__ are non deterministic
            (lambda _
              (let ((pycaches (find-files "linux-package/"
                                          "__pycache__"
                                          #:directories? #t)))
                (for-each delete-file-recursively pycaches))))
          (replace 'install
            (lambda _
              (let* ((obin (string-append #$output "/bin"))
                     (olib (string-append #$output "/lib"))
                     (oshare (string-append #$output "/share")))
                (copy-recursively "linux-package/bin" obin)
                (copy-recursively "linux-package/share" oshare)
                (copy-recursively "linux-package/lib" olib)))))))
    (synopsis "Fast, featureful, GPU based terminal emulator")
    (description "Kitty is a fast and featureful GPU-based terminal emulator:
@itemize
@item Offloads rendering to the GPU for lower system load and buttery smooth
scrolling.  Uses threaded rendering to minimize input latency.
@item Supports all modern terminal features: graphics (images), unicode,
true-color, OpenType ligatures, mouse protocol, focus tracking, bracketed
paste and several new terminal protocol extensions.
@item Supports tiling multiple terminal windows side by side in different
layouts without needing to use an extra program like tmux.
@item Can be controlled from scripts or the shell prompt, even over SSH.
@item Has a framework for Kittens, small terminal programs that can be used to
extend kitty's functionality.  For example, they are used for Unicode input,
hints, and side-by-side diff.
@item Supports startup sessions which allow you to specify the window/tab
layout, working directories and programs to run on startup.
@item Allows you to open the scrollback buffer in a separate window using
arbitrary programs of your choice.  This is useful for browsing the history
comfortably in a pager or editor.
@end itemize")
    (license license:gpl3+)))

(define-public eternalterminal
  (package
    (name "eternalterminal")
    (version "6.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MisterTea/EternalTerminal")
             (commit (string-append "et-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13vhr701j85ga37d53339bxgrf9fqa6z1zcp6s3ly5bb8p7lyvzm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TEST=ON" "-DDISABLE_VCPKG=1")))
    (inputs (list libsodium protobuf openssl zlib curl))
    (home-page "https://mistertea.github.io/EternalTerminal/")
    (synopsis "Remote shell that reconnects without interrupting the session")
    (description "@dfn{Eternal Terminal} (ET) is a remote shell that
automatically reconnects without interrupting the session.  ET uses SSH to
initialize a secure connection.  Unlike SSH sessions, which must be killed and
reconnected after a network outage an ET session will survive network outages
and IP roaming.  ET provides the same core functionality as @command{mosh},
while also supporting native scrolling and @command{tmux} control mode
(@code{tmux -CC}).")
    (license license:asl2.0)))

(define-public wterm
  (deprecated-package "wterm" foot))

(define-public tilix
  (package
    (name "tilix")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnunn1/tilix")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vq0igfq1hj017ivfkd03zbb620qhvcjn9vd56c5dr4r1j7jiz98"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env-variables
            (lambda _
              (setenv "CC"
                      #$(cc-for-target))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson_post_install.py"
                (("gtk-update-icon-cache") (which "true"))
                (("update-desktop-database") (which "true"))))))))
    (inputs (list dbus
                  dconf
                  gettext-minimal
                  gsettings-desktop-schemas
                  gtkd
                  gtk+
                  libsecret
                  libunwind
                  vte/gtk+-3))
    (native-inputs (list appstream
                         desktop-file-utils
                         `(,glib "bin")
                         `(,gtk+ "bin")
                         ldc
                         pkg-config))
    (home-page "https://gnunn1.github.io/tilix-web/")
    (synopsis "Tiling terminal emulator")
    (description
     "Tilix is a tiling terminal emulator following the
Gnome Human Interface Guidelines.  Its features include:
@enumerate
@item Layout terminals in any fashion by splitting them horizontally or
vertically.
@item Terminals can be re-arranged using drag and drop both within and between
windows.
@item Terminals can be detached into a new window via drag and drop.
@item Input can be synchronized between terminals so commands typed in one
terminal are replicated to the others.
@item Supports notifications when processes are completed out of view.
@end enumerate")
    (license license:mpl2.0)))

(define-public tio
  (package
    (name "tio")
    (version "3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tio/tio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "050zm7nh9niag1amjql859cj3xc9gbidk3zz546h6fhhh3vykmfl"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list glib lua))
    (arguments
     (list
      #:configure-flags
      #~(list "-Dbashcompletiondir=share/bash-completion/completions")))
    (home-page "https://tio.github.io/")
    (synopsis "Simple TTY terminal I/O application")
    (description "tio is a simple TTY terminal application which features a
straightforward commandline interface to easily connect to TTY devices for
basic input/output.")
    (license license:gpl2+)))

(define-public alacritty
  (package
    (name "alacritty")
    (version "0.15.0")
    (source
     (origin
       ;; XXX: The crate at "crates.io" contains only the alacritty subproject
       ;; of alacritty and thus has limited contents.  In particular,
       ;; it does not contain "extra" directory with completions, icon, etc.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alacritty/alacritty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nh5w037rwf00z9b21803184j561s44js9ilfq9pcqbgbg95y308"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-test-flags
       '("--"
         ;; Changes in clap regularly break this test.
         "--skip=cli::tests::completions")
       #:cargo-inputs
       ,(list rust-ahash-0.8
              rust-base64-0.22
              rust-bitflags-2
              rust-clap-4
              rust-copypasta-0.10
              rust-crossfont-0.8
              rust-dirs-5
              rust-embed-resource-2
              rust-gl-generator-0.14
              rust-glutin-0.32
              rust-home-0.5
              rust-libc-0.2
              rust-log-0.4
              rust-miow-0.6
              rust-notify-6
              rust-objc2-0.5
              rust-objc2-app-kit-0.2
              rust-objc2-foundation-0.2
              rust-parking-lot-0.12
              rust-piper-0.2
              rust-polling-3
              rust-png-0.17
              rust-proc-macro2-1
              rust-quote-1
              rust-regex-automata-0.4
              rust-rustix-openpty-0.1
              rust-serde-1
              rust-serde-json-1
              rust-serde-yaml-0.9
              rust-signal-hook-0.3
              rust-syn-2
              rust-tempfile-3
              rust-toml-0.8
              rust-toml-edit-0.22
              rust-unicode-width-0.1
              rust-vte-0.13
              rust-windows-sys-0.52
              rust-winit-0.30
              rust-xdg-2)
       #:cargo-development-inputs
       ,(list rust-clap-complete-4
              rust-log-0.4
              rust-serde-1
              rust-serde-json-1
              rust-toml-0.8)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-xdg-open
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "alacritty/src/config/ui_config.rs"
               (("xdg-open") (search-input-file inputs "/bin/xdg-open")))))
         (add-after 'configure 'add-absolute-library-references
           (lambda* (#:key inputs vendor-dir #:allow-other-keys)
             ;; Fix dlopen()ing some libraries on pure Wayland (no $DISPLAY):
             ;; Failed to initialize any backend! Wayland status: NoWaylandLib
             ;; XXX We patch transitive dependencies that aren't even direct
             ;; inputs to this package, because of the way Guix's Rust build
             ;; system currently works.  <http://issues.guix.gnu.org/46399>
             ;; might fix this and allow patching them directly.
             (substitute* (find-files vendor-dir "\\.rs$")
               (("libEGL\\.so")
                (search-input-file inputs "lib/libEGL.so"))
               (("libGL\\.so")
                (search-input-file inputs "lib/libGL.so"))
               ;; Lots of libraries from rust-x11-dl and others.
               (("libX[[:alpha:]]*\\.so" all)
                (search-input-file inputs (string-append "lib/" all)))

               ;; There are several libwayland libraries.
               (("libwayland\\.so" all)
                (search-input-file inputs (string-append "lib/" all)))
               (("libwayland-[[:alpha:]]*\\.so" all)
                (search-input-file inputs (string-append "lib/" all)))
               (("libxkbcommon-x11\\.so")
                (search-input-file inputs "lib/libxkbcommon-x11.so"))
               (("libxkbcommon\\.so")
                (search-input-file inputs "lib/libxkbcommon.so")))))
         (replace 'install
           ;; Upstream install script only takes care of executable.
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (icons (string-append share "/icons/hicolor/scalable/apps"))
                    (tic   (search-input-file (or native-inputs inputs) "/bin/tic"))
                    (man   (string-append share "/man"))
                    (alacritty-bin (car (find-files "target" "^alacritty$"))))
               ;; Install the executable.
               (install-file alacritty-bin bin)
               ;; Install man pages.
               (mkdir-p (string-append man "/man1"))
               (mkdir-p (string-append man "/man5"))
               (define (create-manpage manpage)
                 (let ((mandir (string-append
                                 "/man" (string-take-right manpage 1) "/")))
                   (with-input-from-file (string-append manpage ".scd")
                     (lambda _
                       (with-output-to-file (string-append man mandir manpage)
                         (lambda _ (invoke "scdoc")))))))
               (with-directory-excursion "extra/man"
                 (for-each create-manpage '("alacritty.1"
                                            "alacritty-msg.1"
                                            "alacritty.5"
                                            "alacritty-bindings.5")))
               ;; Install desktop file.
               (install-file "extra/linux/Alacritty.desktop"
                             (string-append share "/applications"))
               (install-file "extra/linux/org.alacritty.Alacritty.appdata.xml"
                             (string-append share "/metainfo"))
               ;; Install icon.
               (mkdir-p icons)
               (copy-file "extra/logo/alacritty-term.svg"
                          (string-append icons "/Alacritty.svg"))
               ;; Install terminfo.
               (mkdir-p (string-append share "/terminfo"))
               ;; We don't compile alacritty-common entry because
               ;; it's being used only for inheritance.
               (invoke tic "-x" "-e" "alacritty,alacritty-direct"
                       "-o" (string-append share "/terminfo/")
                       "extra/alacritty.info")
               ;; Install completions.
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (copy-file "extra/completions/alacritty.bash"
                          (string-append out "/etc/bash_completion.d/alacritty"))
               (install-file "extra/completions/_alacritty"
                             (string-append share "/zsh/site-functions"))
               (install-file "extra/completions/alacritty.fish"
                             (string-append share "/fish/vendor_completions.d"))))))))
    (native-inputs
     (list ncurses
           pkg-config
           python
           scdoc))
    (inputs
     (list expat
           fontconfig
           freetype
           libx11
           libxcb
           libxcursor
           libxext
           libxft
           libxi
           libxinerama
           libxkbcommon
           libxmu
           libxpresent
           libxrandr
           libxscrnsaver
           libxt
           libxtst
           libxxf86vm
           mesa
           xdg-utils
           wayland))
    (native-search-paths
     ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
     ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (home-page "https://alacritty.org/")
    (synopsis "GPU-accelerated terminal emulator")
    (description
     "Alacritty is a GPU-accelerated terminal emulator with a strong focus on
simplicity and performance.  With such a strong focus on performance, included
features are carefully considered and you can always expect Alacritty to be
blazingly fast.  By making sane choices for defaults, Alacritty requires no
additional setup.  However, it does allow configuration of many aspects of the
terminal.  Note that you need support for OpenGL 3.2 or higher.")
    (license license:asl2.0)))

(define-public bootterm
  (package
    (name "bootterm")
    (version "0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wtarreau/bootterm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xag6agcqkq2p7gp20qxjb95ah7p6lia65jmm5v51rqxfzclx2h1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; no test suite
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               ;; No ./configure script
               (delete 'configure)
               (add-after 'install 'install-doc
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((doc (format #f "~a/share/doc/~a-~a"
                                      #$output #$name #$version)))
                     (install-file "README.md" doc)))))))
    (home-page "https://github.com/wtarreau/bootterm")
    (synopsis "Serial terminal")
    (description "Bootterm is a terminal designed to ease connection to
ephemeral serial ports.  It features automatic port detection, port enumeration,
support for non-standard baud rates, the ability to wait for ports to appear,
and the ability to read and write via stdin and stdout.")
    (license license:expat)))

(define-public roxterm
  (package
    (name "roxterm")
    (version "3.15.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/realh/roxterm")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jsdrs31mwpba851inwxpwnmy74k9nl4hs7bgbhba85dvqpw1xi2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; No tests
    (native-inputs
     (list docbook-xsl
           docbook-xml
           (list glib "bin")
           libxml2
           libxslt
           pkg-config))
    (inputs
     (list dbus dbus-glib gtk+ pcre vte/gtk+-3))
    (synopsis "Terminal emulator")
    (description "This package provides a terminal emulator with hyperlink
support.  It's based on VTE and aimed at power users.")
    (home-page "https://realh.github.io/roxterm/en/index.html")
    ;; src/gresources.c is under LGPL 2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))
