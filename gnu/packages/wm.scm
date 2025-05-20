;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015, 2016, 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2019, 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2016 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2020 Nikita <nikita@n0.is>
;;; Copyright © 2016 doncatnip <gnopap@gmail.com>
;;; Copyright © 2016 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019 Ingo Ruhnke <grumbel@gmail.com>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2019 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019 Noodles! <nnoodle@chiru.no>
;;; Copyright © 2019, 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Boris A. Dekshteyn <harlequin78@gmail.com>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2022, 2023 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020 Niklas Eklund <niklas.eklund@posteo.net>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2021, 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 lasnesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021, 2023 jgart <jgart@dismail.de>
;;; Copyright © 2021 Disseminate Dissent <disseminatedissent@protonmail.com>
;;; Copyright © 2022, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Pier-Hugues Pellerin <ph@heykimo.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Elais Player <elais@fastmail.com>
;;; Copyright © 2022, 2023 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2022 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2023 Jonathan Brielamier <jonathan.brielmaier@web.de>
;;; Copyright © 2023 Vessel Wave <vesselwave@disroot.org>
;;; Copyright © 2023, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023, 2024 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2024 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2024 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2024 chris <chris@bumblehead.com>
;;; Copyright © 2024 Erik Eduardo Alonso Hernández <erik@erikeduardo.xyz>
;;; Copyright © 2024 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2024 bigbug <bigbookofbug@proton.me>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2024 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Josep Bigorra <jjbigorra@gmail.com>
;;; Copyright © 2024 Jakob Kirsch <jakob.kirsch@web.de>
;;; Copyright © 2025 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Junker <dk@junkeria.club>
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

(define-module (gnu packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public bspwm
  (package
    (name "bspwm")
    (version "0.9.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/bspwm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qlv7b4c2mmjfd65y100d11x8iqyg5f6lfiws3cgmpjidhdygnxc"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcb
           libxinerama
           sxhkd
           xcb-util
           xcb-util-keysyms
           xcb-util-wm))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))           ; no configure script
           #:tests? #f                      ; no check target
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))))
    (home-page "https://github.com/baskerville/bspwm")
    (synopsis "Tiling window manager based on binary space partitioning")
    (description "bspwm is a tiling window manager that represents windows as
the leaves of a full binary tree.")
    (license license:bsd-2)))

(define-public cage
  (package
    (name "cage")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cage-kiosk/cage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "0y7vqyvzphpzm0bnkrhs7qqbjpcb0sn0nlwif9y43l5kmp7ns8fr"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config scdoc
                         ;; for wayland-scanner
                         wayland))
    (inputs (list wayland wlroots libxkbcommon))
    (home-page "https://github.com/cage-kiosk/cage")
    (synopsis "Wayland kiosk")
    (description "This package provides a Wayland @dfn{kiosk}, which runs a
single, maximized application.")
    (license license:expat)))

(define-public herbstluftwm
  (package
    (name "herbstluftwm")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://herbstluftwm.org/tarballs/herbstluftwm-"
                           version ".tar.gz"))
       (sha256
        (base32 "01c1f5041bblg8d7p12jkynd57xi1frxy61qsrdcxgp5144n1m5j"))
       (file-name (string-append "herbstluftwm-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (inputs
     (list dzen
           dmenu
           glib
           glibmm
           xterm
           xsetroot
           libx11
           libxcursor
           libxext
           libxfixes
           libxinerama
           libxrandr
           libxft))
    (native-inputs
     (list asciidoc pkg-config python))
    (arguments
     '(#:tests? #f
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "-DCC=gcc"
               (string-append "-DCMAKE_INSTALL_SYSCONF_PREFIX=" out "/etc")
               (string-append "-DBASHCOMPLETIONDIR=" out "/etc/bash_completion.d")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'link-libxcursor
           (lambda _
             ;; libX11 will dlopen libXcursor to load cursors.
             (setenv "LDFLAGS" "-lXcursor")))
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                   (string-append xsessions "/herbstluftwm.desktop")
                 (lambda (port)
                   (format port "~
                     [Desktop Entry]~@
                     Name=herbstluftwm~@
                     Comment=Manual tiling window manager~@
                     Exec=~a/bin/herbstluftwm~@
                     Type=XSession~%" out)))
               #t))))))
    (synopsis "Tiling window manager for X11")
    (description "herbstluftwm is a manual tiling window manager for X11 using
Xlib and GLib.  Its main features are:

@itemize
@item
The layout is based on splitting frames into subframes which can be split
again or can be filled with windows (similar to i3 or musca).

@item
Tags (or workspaces or virtual desktops or …) can be added/removed at runtime.
Each tag contains an own layout.

@item
Exactly one tag is viewed on each monitor.  The tags are monitor independent
(similar to Xmonad).

@item
It is configured at runtime via IPC calls from @command{herbstclient}.  So the
configuration file is just a script which is run on startup (similar to wmii
or musca).

@end itemize")
    (home-page "https://herbstluftwm.org")
    (license license:bsd-2)))

(define-public hypridle
  ;; Go back to regular versioning on next release.
  (let ((commit "4f1c165d3e340331de020b46b33a3edb2fd9d55e")
        (revision "1"))
    (package
      (name "hypridle")
      (version (git-version "0.1.6" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/hypridle")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1f75vfd5fv8zhd7hy7lg26wmlaslxqj2knf3zi6wnv21n63m3wa1"))))
      (build-system cmake-build-system)
      (arguments (list #:tests? #f)) ;No tests.
      (native-inputs (list gcc-14 pkg-config))
      (inputs
       (list hyprland-protocols
             hyprlang
             hyprutils
             hyprwayland-scanner
             sdbus-c++
             wayland
             wayland-protocols))
      (home-page "https://github.com/hyprwm/hypridle")
      (synopsis "Hyprland's idle daemon")
      (description "Hyprland's idle daemon, based on the
@code{ext-idle-notify-v1} Wayland protocol.  Hypridle has support for D-Bus's
loginctl commands (lock/unlock/before-sleep) and inhibit.")
      (license license:bsd-3))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.49.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources and hyprpm utility.
                  (substitute* "CMakeLists.txt"
                    (("^add_subdirectory\\(hyprpm\\).*") ""))
                  (for-each delete-file-recursively
                            '("hyprpm"
                              "subprojects"))))
              (sha256
               (base32
                "0c2pvi9cdg6jv9wiz966q1sj8mjmxsgvcplsmfhhknpy7h2gp5px"))))
    (build-system cmake-build-system)
    (arguments
     (list #:cmake cmake-next
           #:tests? #f                  ;No tests.
           #:configure-flags #~'("-DNO_HYPRPM=True")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/xwayland/Server.cpp"
                     (("Xwayland( \\{\\})" _ suffix)
                      (string-append
                       (search-input-file inputs "bin/Xwayland")
                       suffix)))
                   (substitute* (find-files "src" "\\.cpp$")
                     (("/usr/local(/bin/Hyprland)" _ path)
                      (string-append #$output path))
                     (("/usr") #$output)
                     (("\\<(addr2line|cat|lspci|nm)\\>" cmd)
                      (search-input-file
                       inputs (string-append "bin/" cmd))))
                   (substitute* '("src/Compositor.cpp"
                                  "src/xwayland/XWayland.cpp"
                                  "src/managers/VersionKeeperManager.cpp")
                     (("!NFsUtils::executableExistsInPath.*\".") "false")
                     (("hyprland-update-screen" cmd)
                      (search-input-file inputs (in-vicinity "bin" cmd)))))))))
    (native-inputs
     (list gcc-14
           hyprwayland-scanner
           (module-ref (resolve-interface
                  '(gnu packages commencement))
                 'ld-wrapper)
           pkg-config))
    (inputs
     (list aquamarine
           binutils
           cairo
           hyprcursor
           hyprgraphics
           hyprland-protocols
           hyprland-qtutils
           hyprlang
           hyprutils
           libinput-minimal
           libxcursor
           libxkbcommon
           mesa
           pango
           pciutils
           re2-next
           udis86
           wayland
           wayland-protocols-next
           linux-libre-headers-6.14
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (home-page "https://hyprland.org/")
    (synopsis "Dynamic tiling Wayland compositor")
    (description
     "Hyprland is a dynamic tiling Wayland compositor that doesn't sacrifice on
its looks.")
    (properties
     `((upstream-name . "source")))
    (license license:bsd-3)))

(define-public i3status
  (package
    (name "i3status")
    (version "2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/i3status/i3status-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0929chhvyq9hg4scpcz8r9zn3s9jvbg6a86k3wqa77qg85rh4kaw"))
              (snippet
               ;; Delete the pregenerated man page, to be rebuilt from source.
               '(delete-file "man/i3status.1"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           '(list "-Dmans=True")
           #:tests? #f))                ; no test suite
    (inputs
     (list alsa-lib
           libconfuse
           libnl
           yajl
           pulseaudio))
    (native-inputs
     (list asciidoc
           perl
           pkg-config
           docbook-xsl
           xmlto))
    (home-page "https://i3wm.org/i3status/")
    (synopsis "Status bar for i3bar, dzen2, xmobar or similar programs")
    (description "i3status is a small program for generating a status bar for
i3bar, dzen2, xmobar or similar programs.  It is designed to be very efficient
by issuing a very small number of system calls, as one generally wants to
update such a status line every second.  This ensures that even under high
load, your status bar is updated correctly.  Also, it saves a bit of energy by
not hogging your CPU as much as spawning the corresponding amount of shell
commands would.")
    (license license:bsd-3)))

(define-public i3-wm
  (package
    (name "i3-wm")
    (version "4.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/downloads/i3-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0jrya4rhh46sivlmqaqc4n9abpp1yn1ajhi616gn75cxwl8rjqr8"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; The test suite requires the unpackaged Xephyr X server.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-session-file
            (lambda _
              (let ((i3 (string-append #$output "/bin/i3"))
                    (i3-with-shmlog (string-append #$output "/bin/i3-with-shmlog")))
                (substitute* (string-append #$output "/share/xsessions/i3.desktop")
                  (("Exec=i3") (string-append "Exec=" i3)))
                (substitute* (string-append #$output "/share/xsessions/i3-with-shmlog.desktop")
                  (("Exec=i3-with-shmlog") (string-append "Exec=" i3-with-shmlog))))))
           (add-after 'patch-session-file 'wrap-perl-bin
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((i3-save-tree (string-append #$output "/bin/i3-save-tree"))
                      (perl-lib-names '("perl-anyevent"
                                        "perl-anyevent-i3"
                                        "perl-json-xs"
                                        "perl-common-sense"
                                        "perl-types-serialiser"))
                      (perl-lib-paths
                       (map (lambda (name)
                              (string-append (assoc-ref inputs name) "/lib/perl5/site_perl"))
                            perl-lib-names)))
                 (wrap-program i3-save-tree
                               `("PERL5LIB" ":" prefix ,perl-lib-paths))))))))
    (inputs
     (list libxcb
           xcb-util
           xcb-util-cursor
           xcb-util-keysyms
           xcb-util-wm
           xcb-util-xrm
           libxkbcommon
           libev
           yajl
           xmlto
           perl
           perl-anyevent-i3
           perl-json-xs
           perl-common-sense
           perl-types-serialiser
           perl-pod-simple
           libx11
           pcre2
           startup-notification
           pango
           cairo))
    (native-inputs
     (list which
           perl
           pkg-config
           asciidoc
           ;; For building the documentation.
           libxml2
           docbook-xsl))
    (home-page "https://i3wm.org/")
    (synopsis "Tiling window manager")
    (description "i3 is a tiling X11 window manager that dynamically manages
tiled, stacked, and tabbed window layouts.

i3 primarily targets advanced users.  Windows are managed manually and organised
inside containers, which can be split vertically or horizontally, and optionally
resized.

i3 uses a plain-text configuration file, and can be extended and controlled from
many programming languages.")
    (properties
     `((upstream-name . "i3")
       (release-monitoring-url . "https://i3wm.org/downloads")))
    (license license:bsd-3)))

(define-public i3-gaps
  (deprecated-package "i3-gaps" i3-wm))

(define-public i3ipc-glib
  (package
    (name "i3ipc-glib")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/altdesktop/i3ipc-glib")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01fzvrbnzcwx0vxw29igfpza9zwzp2s7msmzb92v01z0rz0y5m0p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list
      autoconf
      automake
      `(,glib "bin")                    ;for glib-mkenums
      gobject-introspection
      gtk-doc
      libtool
      pkg-config
      which))
    (propagated-inputs
     ;; In Requires.private of i3ipc-glib-1.0.pc.
     (list
      glib
      json-glib
      libxcb))
    (home-page "https://github.com/altdesktop/i3ipc-glib")
    (synopsis "C interface library to i3 window manager")
    (description
     "@code{i3ipc-GLib} is a C library for controlling the i3 window manager.")
    (license license:gpl3+)))

(define-public i3lock
  (package
    (name "i3lock")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/i3lock/i3lock-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "02szjsaz7rqrdkd0r2nwgwa85c4hwfrcskxw7ryk695kmjcfhzv3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           libev
           linux-pam
           libxcb
           libxkbcommon
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (home-page "https://i3wm.org/i3lock/")
    (synopsis "Lightweight screen locker")
    (description
     "i3lock is a simple X11 screen locker developed alongside the i3 project.
Despite the name it should work with any X11 window manager.")
    (license license:bsd-3)))

(define-public i3lock-blur
  (package
    (name "i3lock-blur")
    (version "2.10")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/karulont/i3lock-blur")
                (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bd5nrlga5g1sz1f64gnc3dqy8yfrr4q1ss59krymbpxa1hhf55c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config automake autoconf))
    (inputs
     (list cairo
           mesa
           libev
           linux-pam
           libxcb
           libxkbcommon
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (home-page "https://github.com/karulont/i3lock-blur")
    (synopsis "Lightweight screen locker with transparent blurring background")
    (description
     "Simple X11 screen locker with transparent blurring background developed
alongside the i3 project.  Despite the name it should work with any X11 window
manager.")
    (license license:expat)))

(define-public i3blocks
  (package
    (name "i3blocks")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vivien/i3blocks")
                    (commit version)))
              (sha256
               (base32
                "0v8mwnm8qzpv6xnqvrk43s4b9iyld4naqzbaxk4ldq1qkhai0wsv"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://github.com/vivien/i3blocks")
    (synopsis "Minimalist scheduler for status bar scripts")
    (description "i3blocks executes your command lines and generates a
status line from their output.  The generated line is meant to be displayed by
the i3 window manager through its i3bar component, as an alternative to
i3status.")
    (license license:gpl3+)))

(define-public papersway
  (package
    (name "papersway")
    (version "2.000")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/S/SP/SPWHITTON/App-papersway-" version
             ".tar.gz"))
       (sha256
        (base32 "0mphgyi6gq98g4n0jq5qwf66qi76rbrwgipmqqfzsakc3rzbsivh"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-program
                 (lambda _
                   (for-each
                    (lambda (command)
                      (wrap-program (string-append #$output "/bin/" command)
                        `("PERL5LIB" ":" prefix
                          (,(getenv "PERL5LIB")
                           ,(string-append #$output "/lib/perl5/site_perl")))))
                    '("papersway" "papersway-msg")))))))
    (inputs (list perl-anyevent perl-anyevent-i3 perl-json))
    (home-page "https://spwhitton.name/tech/code/papersway/")
    (synopsis
     "Scrollable tiling window management for Sway and i3 window manager")
    (description
     "@command{papersway} is an implementation of scrollable window management
like @code{gnome-shell-extension-paperwm} for @code{sway} and @code{i3-wm}.
If you like @code{sway} and @code{i3-wm}'s commitments to stability, avoiding
scope creep etc., but dislike the window management model, @command{papersway}
might be of interest.")
    (license license:gpl3)))

(define-public perl-anyevent-i3
  (package
    (name "perl-anyevent-i3")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MS/MSTPLBG/"
                                  "AnyEvent-I3-" version ".tar.gz"))
              (sha256
               (base32
                "0fj8mhfh9z4zgccpfpm8ymj245zii8z3b4g7ila60m9xvdh3pk8v"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-anyevent perl-json-xs))
    (home-page "https://metacpan.org/release/AnyEvent-I3")
    (synopsis
     "Communicate with the i3 window manager through perl")
    (description
     "This module connects to the i3 window manager using the UNIX socket
based IPC interface it provides (if enabled in the configuration file).
You can then subscribe to events or send messages and receive their replies.")
    ;; Can be used with either license.
    (license (list license:gpl3+ license:perl-license))))

(define-public python-i3-py
  (package
    (name "python-i3-py")
    (version "0.6.5")
    (source
     (origin
       ;; The latest release is not tagged in Git nor has an entry in PyPi,
       ;; but there is still a clear commit for it, and it's been the last one
       ;; for years.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziberna/i3-py")
             (commit "27f88a616e9ecc340e7d041d3d00782f8a1964c1")))
       (sha256
        (base32
         "1nm719dc2xqlll7vj4c4m7mpjb27lpn3bg3c66gajvnrz2x1nmxs"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests yet
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-doc
                    ;; Copy readme file to documentation directory.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "out")
                                                "/share/doc/" ,name)))
                        (install-file "README.md" doc)
                        ;; Avoid unspecified return value.
                        #t))))))
    (propagated-inputs
     (list i3-wm))
    (home-page "https://github.com/ziberna/i3-py")
    (synopsis "Python interface to the i3 window manager")
    (description "This package allows you to interact from a Python program
with the i3 window manager via its IPC socket.  It can send commands and other
kinds of messages to i3, select the affected containers, filter results and
subscribe to events.")
    (license license:gpl3+)))

(define-public qtile
  (package
    (name "qtile")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qtile" version))
       (sha256
        (base32 "0zd2bh4mvgwjxkkwn3angkaqzm7ldcmzg3gdc098jzzlf90fmywm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; A lot of tests fail despite Xvfb and writable temp/cache space.
      #:tests? #f
      #:test-flags
      #~(list "--ignore=test/widgets/test_widget_init_configure.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "libqtile/pangocffi.py"
                (("^(gobject = ffi.dlopen).*" all def)
                 (format #f "~a(~s)~%" def
                         (search-input-file inputs "/lib/libgobject-2.0.so.0")))
                (("^(pango = ffi.dlopen).*" all def)
                 (format #f "~a(~s)~%" def
                         (search-input-file inputs "/lib/libpango-1.0.so.0")))
                (("^(pangocairo = ffi.dlopen).*" all def)
                 (format #f "~a(~s)~%" def
                         (search-input-file
                          inputs "/lib/libpangocairo-1.0.so.0"))))))
          (add-after 'install 'install-xsessions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions"))
                     (qtile (string-append out "/bin/qtile start")))
                (mkdir-p xsessions)
                (copy-file "resources/qtile.desktop"
                           (string-append xsessions "/qtile.desktop"))
                (substitute* (string-append xsessions "/qtile.desktop")
                  (("qtile start") qtile)))))
          (add-before 'check 'pre-check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1")
                (setenv "XDG_CACHE_HOME" "/tmp")))))))
    (inputs
     (list glib pango pulseaudio))
    (propagated-inputs
     (list python-cairocffi
           python-cffi
           python-dateutil
           python-dbus-fast
           python-iwlib
           python-keyring
           python-libcst
           python-mpd2
           python-pygobject
           python-pyxdg
           python-xcffib))
    (native-inputs
      (list pkg-config
            python-flake8
            python-pep8-naming
            python-pytest
            python-pytest-cov
            python-psutil
            python-setuptools
            python-setuptools-scm
            python-wheel
            xorg-server-for-tests))
    (home-page "http://qtile.org")
    (synopsis "Hackable tiling window manager written and configured in Python")
    (description "Qtile is simple, small, and extensible.  It's easy to write
your own layouts, widgets, and built-in commands.")
    (license license:expat)))

(define-public quickswitch-i3
  (let ((commit "ed692b1e8f43b95bd907ced26238ce8ccb2ed28f")
        (revision "1")) ; Guix package revision
    (package
      (name "quickswitch-i3")
      (version (string-append "2.2-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         ;; The latest commit is a few years old and just a couple commits
         ;; after the last tagged release, so we use that latest commit
         ;; instead of the release.
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/proxypoke/quickswitch-for-i3")
               (commit commit)))
         (sha256
          (base32
           "0447077sama80jcdg5p64zjsvafmz5rbdrirhm1adcdjhkh6iqc5"))
         (patches (search-patches "quickswitch-fix-dmenu-check.patch"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no tests yet
         #:phases (modify-phases %standard-phases
                    (add-after 'install 'install-doc
                      ;; Copy readme file to documentation directory.
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((doc (string-append (assoc-ref outputs "out")
                                                  "/share/doc/" ,name)))
                          (install-file "README.rst" doc)
                          ;; Avoid unspecified return value.
                          #t))))))
      (inputs
       (list python-i3-py dmenu))
      (home-page "https://github.com/proxypoke/quickswitch-for-i3")
      (synopsis "Quickly change to and locate windows in the i3 window manager")
      (description
       "This utility for the i3 window manager allows you to quickly switch to
and locate windows on all your workspaces, using an interactive dmenu
prompt.")
      (license license:wtfpl2))))

(define-public i3lock-color
  (package
    (name "i3lock-color")
    (version "2.13.c.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Raymo111/i3lock-color")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lnyh8spbf1ar4xan5v7q8i2i51aq1i60kzbfkn9w3wa0jzf9f3d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     (list cairo
           libev
           libjpeg-turbo
           libxcb
           libxkbcommon
           linux-pam
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://github.com/Raymo111/i3lock-color")
    (synopsis "Screen locker with color configuration support")
    (description
     "i3lock-color is a simpler X11 screen locker derived from i3lock.
Features include:

@enumerate
@item forking process, the locked screen is preserved when you suspend to RAM;
@item specify background color or image to be displayed in the lock screen;
@item many additional color options.
@end enumerate")
    (license license:bsd-3)))

(define-public i3lock-fancy
  (package
    (name "i3lock-fancy")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meskarune/i3lock-fancy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11g2kkim33ra38d1m897sq1ifajw17iyw9mr7sg1q8i2ibl4lfsi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (icons (string-append out "/share/i3lock-fancy/icons/"))
                    (wmctrl (search-input-file inputs "/bin/wmctrl"))
                    (mconvert (search-input-file inputs "/bin/convert"))
                    (mimport (search-input-file inputs "/bin/import"))
                    (awk (search-input-file inputs "/bin/gawk")))

               (substitute* "lock"
                 (("\\$\\(command -V wmctrl\\)") wmctrl)
                 (("convert") mconvert)
                 (("shot=\\(import") (string-append "shot=\(" mimport))
                 (("awk -F") (string-append awk " -F"))
                 ((" awk") awk)
                 (("\\$scriptpath/icons/") icons))
               #t)))
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (icons (string-append out "/share/i3lock-fancy/icons/")))

               (install-file "lock" bin)
               (rename-file (string-append bin "/lock")
                            (string-append bin "/i3lock-fancy"))
               (copy-recursively "icons" icons)
               #t))))))
    (inputs
     (list imagemagick wmctrl i3lock gawk))
    (home-page "https://github.com/meskarune/i3lock-fancy")
    (synopsis "Screen locker with screenshot function")
    (description
     "@code{i3lock-fancy} is a Bash script that takes a screenshot of
the desktop, blurs the background and adds a lock icon and text.
It requires @code{i3lock-color} or @code{i3lock} and can optionally
be passed any screenshot util like @code{scrot}.
This screen locker can be used with any window manager or
desktop environment.")
    (license license:expat)))

(define-public icewm
  (package
    (name "icewm")
    (version "3.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ice-wm/icewm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x6rszjamswjljzl4sz4fn46apx5n4xnlwkrx0h78m95np3hrdln"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config gettext-minimal))
    (inputs (list fontconfig
                  fribidi
                  glib                  ;for icewm-menu-fdo
                  imlib2
                  libice
                  libjpeg-turbo
                  (librsvg-for-system)  ;for svg support
                  libsm
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxft
                  libxinerama
                  libxpm
                  libxrandr
                  libxrender
                  libx11
                  lzip
                  perl))
    (arguments
     (list #:configure-flags
           #~(list "-DCONFIG_LIBRSVG=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-failing-test
                 ;; strtest.cc tests failing due to $HOME and /etc setup
                 ;; difference under guix
                 (lambda _
                   (substitute* "src/CMakeLists.txt"
                     (("add_test\\(strtest \\$\\{CMAKE_BINARY_DIR\\}/strtest\\)")
                      "")))))))
    (home-page "https://ice-wm.org/")
    (synopsis "Window manager for the X Window System")
    (description
     "IceWM is a window manager for the X Window System.  The goal of IceWM is
speed, simplicity, and not getting in the user’s way.  It comes with a taskbar
with pager, global and per-window keybindings and a dynamic menu system.
Application windows can be managed by keyboard and mouse.  Windows can be
iconified to the taskbar, to the tray, to the desktop or be made hidden.  They
are controllable by a quick switch window (Alt+Tab) and in a window list.  A
handful of configurable focus models are menu-selectable.  Setups with
multiple monitors are supported by RandR and Xinerama.  IceWM is very
configurable, themeable and well documented.  It includes an optional external
background wallpaper manager with transparency support, a simple session
manager and a system tray.")
    (license license:lgpl2.0)))

(define-public xmonad
  (package
    (name "xmonad")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xmonad" version))
       (sha256
        (base32 "1ysxxjkkx2l160nlj1h8ysxrfhxjlmbws2nm0wyiivmjgn20xs11"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmonad")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-xsession
            (lambda _
              (let* ((xsessions (string-append #$output "/share/xsessions"))
                     (entry     (string-append xsessions "/xmonad.desktop")))
                (mkdir-p xsessions)
                (make-desktop-entry-file
                 (string-append xsessions "/xmonad.desktop")
                 #:name "xmonad"
                 #:exec (string-append #$output "/bin/xmonad")
                 #:comment '((#f "xmonad window manager"))
                 #:type "Application")))))))
    (inputs (list ghc-x11 ghc-data-default-class ghc-setlocale))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-classes))
    (home-page "https://xmonad.org")
    (synopsis "Tiling window manager")
    (description
     "Xmonad is a tiling window manager for X.  Windows are arranged
automatically to tile the screen without gaps or overlap, maximising screen
use.  All features of the window manager are accessible from the keyboard: a
mouse is strictly optional.  Xmonad is written and extensible in Haskell.
Custom layout algorithms, and other extensions, may be written by the user in
config files.  Layouts are applied dynamically, and different layouts may be
used on each workspace.  Xinerama is fully supported, allowing windows to be
tiled on several screens.")
    (license license:bsd-3)))

(define-public ghc-xmobar
  (package
    (name "ghc-xmobar")
    (version "0.48.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "xmobar" version))
              (sha256
               (base32
                "1infcisv7l00a4z4byjwjisg4yndk0cymibfii1c7yzyzrlvavhl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmobar")))
    (native-inputs
     (list ghc-hspec hspec-discover))
    (inputs
     (list ghc-alsa-core
           ghc-alsa-mixer
           ghc-dbus
           ghc-extra
           ghc-hinotify
           ghc-http-client-tls
           ghc-http-conduit
           ghc-http-types
           ghc-libmpd
           ghc-netlink
           ghc-cereal
           ghc-old-locale
           ghc-parsec-numbers
           ghc-regex-compat
           ghc-temporary
           ghc-timezone-olson
           ghc-timezone-series
           ghc-x11
           ghc-x11-xft
           ghc-cairo
           ghc-pango
           libxpm))
    (arguments
     `(#:configure-flags (list "--flags=all_extensions")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-binaries
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively (string-append (assoc-ref outputs "out") "/bin"))))
         (add-before 'build 'patch-test-shebang
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "test/Xmobar/Plugins/Monitors/AlsaSpec.hs"
               (("/bin/bash") (which "bash")))))
         (add-before 'build 'patch-cairo-path
           (lambda _
             (substitute* "src/Xmobar/X11/CairoSurface.hsc"
               (("cairo/cairo-xlib.h") "cairo-xlib.h")))))))
    (home-page "https://codeberg.org/xmobar/xmobar")
    (synopsis "Haskell library for minimalistic text based status bars")
    (description
     "@code{ghc-xmobar} is the haskell library that @code{xmobar} is based on.
It can be used to extend @code{xmobar} with other Haskell code.")
    (license license:bsd-3)))

(define-public xmobar
  (package
    (inherit ghc-xmobar)
    (name "xmobar")
    (inputs
     (list ghc-xmobar
           libxpm))
    (arguments
     `(#:configure-flags (list "--flags=all_extensions" "exe:xmobar")
       ;; Haddock documentation is for the library.
       #:haddock? #f
       ;; Tests are for the library.
       #:tests? #f))
    (synopsis "Minimalistic text based status bar")
    (description
     "@code{xmobar} is a lightweight, text-based, status bar written in
Haskell.  It was originally designed to be used together with Xmonad, but it
is also usable with any other window manager.  While xmobar is written in
Haskell, no knowledge of the language is required to install and use it.")
    (license license:bsd-3)))

(define-public yeganesh
  (package
    (name "yeganesh")
    (version "2.4")
    (source
     (origin

       (method url-fetch)
       (uri (string-append "http://dmwit.com/yeganesh/yeganesh-" version ".tar.gz"))
       (sha256
        (base32 "04djfyjab3c5y9z9x8zd0xcx0jyy35zq7cl9ddr4ppf6k5ky6iky"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-strict ghc-xdg-basedir))
    (home-page "http://dmwit.com/yeganesh/")
    (synopsis "Small wrapper around dmenu")
    (description "@code{yeganesh} is a small wrapper around demnu.  Like
dmenu, it accepts input on stdin and writes the chosen result on stdout.
Unlike dmenu, it mangles the input before it presents its choices.  In
particular, it displays commonly-chosen options before uncommon ones.")
    (license license:bsd-3)))

(define-public ghc-xmonad-contrib
  (package
    (name "ghc-xmonad-contrib")
    (version "0.18.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "xmonad-contrib" version))
              (sha256
               (base32
                "0ck4hq9yhdzggrs3q4ji6nbg6zwhmhc0ckf9vr9d716d98h9swq5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmonad-contrib")))
    (inputs (list ghc-random ghc-x11 xmonad ghc-utf8-string ghc-x11-xft))
    (native-inputs (list ghc-quickcheck ghc-hspec))
    (home-page "https://xmonad.org/")
    (synopsis "Third party extensions for xmonad")
    (description
     "Third party tiling algorithms, configurations, and scripts to Xmonad, a
tiling window manager for X.")
    (license license:bsd-3)))

(define-public evilwm
  (package
    (name "evilwm")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.6809.org.uk/evilwm/dl/evilwm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jry36qkg2l02v37zvzszxvxm2d8c62z25gks5gdqqjl9ifbpv1j"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxrandr))
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:make-flags (let ((inputs (map (cut assoc-ref %build-inputs <>)
                                       '("libx11" "libxext" "libxrandr")))
                          (join (lambda (proc strs)
                                  (string-join (map proc strs) " ")))
                          (dash-I (cut string-append "-I" <> "/include"))
                          (dash-L (cut string-append "-L" <> "/lib")))
                      `("desktopfilesdir=$(prefix)/share/xsessions"
                        ,(string-append "prefix=" (assoc-ref %outputs "out"))
                        ,(string-append "CPPFLAGS=" (join dash-I inputs))
                        ,(string-append "LDFLAGS=" (join dash-L inputs))))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)))) ;no configure script
    (home-page "https://www.6809.org.uk/evilwm/")
    (synopsis "Minimalist window manager for the X Window System")
    (description
     "evilwm is a minimalist window manager based on aewm, extended to feature
many keyboard controls with repositioning and maximize toggles, solid window
drags, snap-to-border support, and virtual desktops.")
    (license (license:x11-style "file:///README"))))

(define-public fluxbox
  (package
    (name "fluxbox")
    (version "1.3.7")
    (synopsis "Small and fast window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fluxbox/fluxbox/"
                                  version "/fluxbox-" version ".tar.xz"))
              (sha256
               (base32
                "1h1f70y40qd225dqx937vzb4k2cz219agm1zvnjxakn5jkz7b37w"))
              (patches
               (search-patches "fluxbox-1.3.7-no-dynamic-cursor.patch"
                               "fluxbox-1.3.7-gcc.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CPPFLAGS=-U__TIME__") ;ugly, but for reproducibility
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'force-bootstrap
           (lambda _
             (delete-file "configure")))
         (add-after 'install 'install-vim-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (syntax (string-append
                              out "/share/vim/vimfiles/pack/guix/start/fluxbox/syntax")))
               (copy-recursively "3rd/vim/vim/syntax" syntax)
               #t)))
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                 (string-append xsessions "/fluxbox.desktop")
                 (lambda (port)
                   (format port "~
                     [Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a/bin/startfluxbox~@
                     Type=Application~%" ,name ,synopsis out)))
               #t))))))
    (native-inputs
     (list autoconf automake gnu-gettext pkg-config))
    (inputs
     (list freetype
           fribidi
           imlib2
           libx11
           libxcursor
           libxext
           libxft
           libxinerama
           libxpm
           libxrandr
           libxrender))
    (description "Fluxbox is a window manager.  It is light on resources
and easy to handle yet full of features to make an easy and fast desktop
experience.")
    (home-page "http://fluxbox.org/")
    (license license:expat)))

(define-public fbautostart
  (package
    (name "fbautostart")
    (version "2.718281828")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/paultag/fbautostart.git")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "13h6j5khi5axqhflzhayzgvyhxylmk5vsgin235ji440mzd516gz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (synopsis "XDG autostarter for Fluxbox window manager")
    (description "This package provides an autostarter complaint with
the XDG Autostart specification.")
    (home-page "https://github.com/paultag/fbautostart")
    (license license:expat)))

(define-public fnott
  (package
    (name "fnott")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/dnkl/fnott")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1061p4vxm0nb5pk5q5dw3kpj4q6kzw70skd3zijfjwdb6fkayh8b"))))
    (build-system meson-build-system)
    (arguments `(#:build-type "release"
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'patch-dbus-install-dir
                     (lambda _
                       (substitute* "dbus/meson.build"
                         (("^.*pkgconfig.*$") "")))))))
    (native-inputs
     (list pkg-config
           wayland-protocols
           tllist
           scdoc))
    (inputs
     (list wlroots wayland fcft dbus libpng))
    (home-page "https://codeberg.org/dnkl/fnott")
    (synopsis "Keyboard driven and lightweight Wayland notification daemon")
    (description "Fnott is a keyboard driven and lightweight notification daemon
for wlroots-based Wayland compositors.")
    (license license:expat)))

(define-public awesome
  (package
    (name "awesome")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/awesomeWM/awesome-releases/raw/master/"
             "awesome-" version ".tar.xz"))
       (sha256
        (base32 "0lqpw401mkkmp9wgbvrmm45bqq2j9357l4irwdqv6l1305pls9kq"))
       (modules '((guix build utils)
                  (srfi srfi-19)))
       (snippet
        '(begin
           ;; Remove non-reproducible timestamp and use the date of
           ;; the source file instead.
           (substitute* "common/version.c"
             (("__DATE__ \" \" __TIME__")
              (date->string
               (time-utc->date
                (make-time time-utc 0 (stat:mtime (stat "awesome.c"))))
               "\"~c\"")))
           #t))
       (patches
        (search-patches "awesome-reproducible-png.patch"
                        "awesome-4.3-fno-common.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list asciidoc
           docbook-xsl
           doxygen
           gperf
           imagemagick
           lua-ldoc
           pkg-config
           xmlto))
    (inputs
     (list bash-minimal
           cairo
           dbus
           gdk-pixbuf
           glib
           gobject-introspection
           imlib2
           libev
           libxcb
           libxcursor
           libxdg-basedir
           libxkbcommon
           lua
           lua-lgi
           pango
           startup-notification
           xcb-util
           xcb-util-cursor
           xcb-util-image
           xcb-util-keysyms
           xcb-util-renderutil
           xcb-util-xrm
           xcb-util-wm))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 match))
       ;; Let compression happen in our 'compress-documentation' phase
       ;; so that '--no-name' is used, which removes timestamps from
       ;; gzip output.
       #:configure-flags
       '("-DCOMPRESS_MANPAGES=off")
       ;; Building awesome in its source directory is no longer
       ;; supported.
       #:out-of-source? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/awful/completion.lua"
               (("/usr/bin/env")
                ""))
             ;; The build process needs to load Cairo dynamically.
             (let* ((cairo (string-append (assoc-ref inputs "cairo") "/lib"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (lua-dependencies
                     (filter (match-lambda
                               ((label . _) (string-prefix? "lua-" label)))
                             inputs))
                    (lua-path
                     (string-join
                      (map (match-lambda
                             ((_ . dir)
                              (string-append
                               dir "/share/lua/" lua-version "/?.lua;"
                               dir "/share/lua/" lua-version "/?/?.lua")))
                           lua-dependencies)
                      ";"))
                    (lua-cpath
                     (string-join
                      (map (match-lambda
                             ((_ . dir)
                              (string-append
                               dir "/lib/lua/" lua-version "/?.so;"
                               dir "/lib/lua/" lua-version "/?/?.so")))
                           lua-dependencies)
                      ";")))
               (setenv "LD_LIBRARY_PATH" cairo)
               (setenv "LUA_PATH" (string-append "?.lua;" lua-path))
               (setenv "LUA_CPATH" lua-cpath)
               (setenv "HOME" (getcwd))
               (setenv "XDG_CACHE_HOME" (getcwd)))))
         (replace 'check
           (lambda _
             ;; There aren't any tests, so just make sure the binary
             ;; gets built and can be run successfully.
             (invoke "../build/awesome" "-v")))
         (add-after 'install 'patch-session-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (awesome (string-append out "/bin/awesome")))
               (substitute* (string-append out "/share/xsessions/awesome.desktop")
                 (("Exec=awesome") (string-append "Exec=" awesome))))))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((awesome (assoc-ref outputs "out"))
                    (cairo (string-append (assoc-ref inputs "cairo") "/lib"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (lua-lgi (assoc-ref inputs "lua-lgi")))
               (wrap-program (string-append awesome "/bin/awesome")
                 `("LUA_PATH" ";" suffix
                   (,(format #f "~a/share/lua/~a/?.lua" lua-lgi lua-version)))
                 `("LUA_CPATH" ";" suffix
                   (,(format #f "~a/lib/lua/~a/?.so" lua-lgi lua-version)))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
                 `("LD_LIBRARY_PATH" suffix (,cairo)))))))))
    (home-page "https://awesomewm.org/")
    (synopsis "Highly configurable window manager")
    (description
     "Awesome has been designed as a framework window manager.  It is fast, small,
dynamic and extensible using the Lua programming language.")
    (license license:gpl2+)))

(define-public menumaker
  (package
    (name "menumaker")
    (version "0.99.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/menumaker/"
                           "menumaker-" version ".tar.gz"))
       (sha256
        (base32 "0nnnc1awvhq5pplvclap3ha61g9209bca6zqgpsm1f53fq75vs8i"))))
    (build-system gnu-build-system)
    (inputs
     (list python))
    (synopsis "Heuristics-driven menu generator")
    (description
     "MenuMaker is a menu generation utility for a number of X window
managers and desktop environments.  It is capable of finding lots of
installed programs and generating a root menu consistent across all
supported window managers, so one will get (almost) the same menu in
all of them.  Currently supported window managers include:

@enumerate
@item BlackBox
@item Deskmenu
@item FluxBox
@item IceWM
@item OpenBox
@item PekWM
@item WindowMaker
@item XFCE
@end enumerate\n")
    (home-page "https://menumaker.sourceforge.net/")
    (license license:bsd-2)))

(define-public keybinder
  (package
    (name "keybinder")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/kupferlauncher/keybinder"
                           "/releases/download/" name "-3.0-v" version "/"
                           name "-3.0-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0830ihwnalw59pp1xzrp37dn58n8vwb8zasnm4a1h81v3x7dxqz6"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+ gobject-introspection))
    (native-inputs
     (list gtk-doc/stable pkg-config))
    (synopsis "Library for registering global keyboard shortcuts, Gtk3 version")
    (description
     "Keybinder is a library for registering global keyboard shortcuts.
Keybinder works with GTK-based applications using the X Window System.")
    (home-page "https://github.com/kupferlauncher/keybinder")
    (license license:x11)))

(define-public keybinder-3.0
  (deprecated-package "keybinder-3.0" keybinder))

(define-public sandbar
  (package
    (name "sandbar")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kolunmi/sandbar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0912cr2q2kg4nqdwy978kpmdcj2cjz3gnlcb28ny9z3cprxvyvxq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
             (delete 'configure))       ;no configure script
           #:tests? #f             ;no check target
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))))
    (inputs (list fcft wayland))
    (native-inputs (list pkg-config wayland-protocols))
    (synopsis "DWM-like bar for the River Wayland compositor")
    (description "Sandbar is a minimalist DWM-like bar designed for River,
a Wayland compositor.  It is triggered through commands sent via standard
input, providing extensive customization options.  This behavior allows users
to dynamically adjust status text, visibility, and bar location, making
Sandbar an ideal choice for those seeking a lightweight and hackable bar
solution in their Wayland environment.")
    ;;             LICENSE      LICENSE.dtao
    (license (list license:gpl3 license:expat))
    (home-page "https://github.com/kolunmi/sandbar")))

(define-public spectrwm
  (package
    (name "spectrwm")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/conformal/spectrwm")
             (commit
              (string-append "SPECTRWM_"
                             (string-join (string-split version #\.) "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fh2r870djrxm3my2z6wigp0gswgh5gvfa9vxcyh7q488k7b0ljn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(let ((pkg-config (lambda (flag)
                                         (string-append "$(shell pkg-config "
                                          flag " "
                                          "freetype2 xft fontconfig x11 libpng)"))))
                       (list (string-append "CC="
                                            #$(cc-for-target))
                             (string-append "PREFIX=" %output)
                             (string-append "INCS=-I. "
                                            (pkg-config "--cflags"))
                             (string-append "LIBS="
                                            (pkg-config "--libs") " -lm")))
      #:tests? #f ;no test suite
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'change-dir
                     (lambda _
                       (chdir "linux") #t))
                   (add-after 'change-dir 'patch-makefile
                     (lambda _
                       (substitute* "Makefile"
                         (("-g")
                          ""))))
                   (delete 'configure)))) ;no 'configure' exists
    (inputs (list freetype
                  fontconfig
                  libx11
                  libxcb
                  libxcursor
                  libxrandr
                  libxtst
                  libxft
                  xcb-util
                  xcb-util-wm
                  xcb-util-keysyms))
    (native-inputs (list libbsd libxt pkg-config))
    (synopsis "Minimalistic automatic tiling window manager")
    (description
     "Spectrwm is a small dynamic tiling and reparenting window manager for X11.
It is inspired by Xmonad and dwm.  Its major features include:

@itemize
@item Navigation anywhere on all screens with either the keyboard or mouse
@item Customizable status bar
@item Restartable without losing state
@item Quick launch menu
@item Many screen layouts possible with a few simple key strokes
@item Move/resize floating windows
@item Extended Window Manager Hints (@dfn{EWMH}) support
@item Configurable tiling
@item Adjustable tile gap allows for a true one pixel border
@item Customizable colors and border width
@end itemize\n")
    (home-page "https://github.com/conformal/spectrwm")
    (license license:isc)))

(define-public cwm
  (package
    (name "cwm")
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://leahneukirchen.org/releases/cwm-"
                           version ".tar.gz"))
       (sha256
        (base32 "145xjwam11194w2irsvs4z0xgn0jdijxfmx67gqd1n0j8g5wan2a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'install-xsession
            (lambda _
              ;; Add a .desktop file to xsessions.
              (let ((xsessions (string-append #$output "/share/xsessions")))
                (mkdir-p xsessions)
                (make-desktop-entry-file
                 (string-append xsessions "/cwm.desktop")
                 #:name "cwm"
                 #:exec (string-append #$output "/bin/cwm")
                 #:try-exec (string-append #$output "/bin/cwm")
                 #:comment '((#f "OpenBSD Calm Window Manager fork")))))))))
    (native-inputs
     (list bison pkg-config))
    (inputs
     (list libxrandr libxft libxinerama))
    (home-page "https://github.com/leahneukirchen/cwm")
    (synopsis "OpenBSD fork of the Calm Window Manager")
    (description "Cwm is a stacking window manager for X11.  It is an OpenBSD
project derived from the original Calm Window Manager.")
    (license license:isc)))

(define-public dunst
  (package
    (name "dunst")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dunst-project/dunst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajpylwwh5c96hq9649gv4sqa6n5rdac41kicq3xf66pmfyny8bs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no check target
      #:make-flags #~(list (string-append "CC="
                                          #$(cc-for-target))
                           (string-append "PREFIX=" %output)
                           (string-append "SYSCONFDIR=" %output "/etc")
                           ;; Otherwise it tries to install service file
                           ;; to "dbus" store directory.
                           (string-append "SERVICEDIR_DBUS=" %output
                                          "/share/dbus-1/services")
                           "dunstify")
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs (list pkg-config perl ;for pod2man
                         which))
    (inputs (list dbus
                  (librsvg-for-system) ;for svg support
                  glib
                  cairo
                  pango
                  libnotify ;for dunstify
                  libx11
                  libxscrnsaver
                  libxinerama
                  libxrandr
                  libxdg-basedir
                  wayland)) ;for wayland support
    (home-page "https://dunst-project.org/")
    (synopsis "Customizable and lightweight notification daemon")
    (description
     "Dunst is a highly configurable and minimalistic notification daemon.
It provides @code{org.freedesktop.Notifications} D-Bus service, so it is
started automatically on the first call via D-Bus.")
    (license license:bsd-3)))

(define-public dwl
  (package
    (name "dwl")
    (version "0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/dwl/dwl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0404awsx8v9fyk7p2bg3p937sc56ixf8ay465xgvjcnv78hh4apd"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list
              #$(string-append "CC=" (cc-for-target))
                (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))   ; no configure
    (native-inputs
     (list pkg-config))
    (inputs
     (list wlroots))
    (home-page "https://codeberg.org/dwl/dwl")
    (synopsis "Dynamic window manager for Wayland")
    (description
     "@command{dwl} is a compact, hackable compositor for Wayland based on
wlroots.  It is intended to fill the same space in the Wayland world that dwm
does in X11, primarily in terms of philosophy, and secondarily in terms of
functionality.  Like dwm, dwl is easy to understand and hack on, due to a
limited size and a few external dependencies.  It is configurable via
@file{config.h}.")
    ;;             LICENSE       LICENSE.dwm   LICENSE.tinywl
    (license (list license:gpl3+ license:expat license:cc0))))

(define-public nitrogen
  (package
    (name "nitrogen")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://github.com/l3ib/nitrogen/"
                                  "releases/download/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zc3fl1mbhq0iyndy4ysmy8vv5c7xwf54rbgamzfhfvsgdq160pl"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 gtkmm-2 glib glibmm))
    (native-inputs
     (list pkg-config))
    (home-page "http://projects.l3ib.org/nitrogen/")
    (synopsis "Background browser and setter for X windows")
    (description
     "This package is a background browser and setter for X windows.  It's
features are:

@itemize
@item Multihead and Xinerama aware
@item Recall mode to used via startup script
@item Uses freedesktop.org standard for thumbnails
@item Can set GNOME background
@item Command lie set modes for script use
@item Inotify monitoring of browse directory
@item Lazy loading of thumbnails - conserves memory
@item \"Automatic\" set mode - determines best mode to set an image based on
its size
@item Display preview images in a tiled icon layout
@end itemize")
    (license license:gpl2+)))

(define-public polybar
  (package
    (name "polybar")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/polybar/polybar/releases/"
                           "download/" version "/polybar-" version ".tar.gz"))
       (sha256
        (base32 "03zz2c3ckxqbwixc2qhsnasq4j4sfia71v75li9w97d0bcwavrjx"))))
    (build-system cmake-build-system)
    (arguments
     ;; Test is disabled because it requires downloading googletest from the
     ;; Internet.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               ;; Make polybar find its default configuration file in the
               ;; store.
               (add-after 'unpack 'patch-config-path
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("/etc") (string-append #$output "/etc")))
                   (substitute* "src/utils/file.cpp"
                     (("\"/etc\"") (string-append "\"" #$output "/etc\""))))))))
    (inputs
     (list alsa-lib
           cairo
           i3-wm
           jsoncpp
           libmpdclient
           libnl
           libuv
           libxcb
           pulseaudio
           xcb-proto
           xcb-util
           xcb-util-cursor
           xcb-util-image
           xcb-util-wm
           xcb-util-xrm))
    (native-inputs
     (list pkg-config
           python-sphinx ; for the manual
           python))      ; xcb-proto depends on python 3
    (home-page "https://polybar.github.io/")
    (synopsis "Fast and easy-to-use status bar")
    (description "Polybar aims to help users build beautiful and highly
customizable status bars for their desktop environment.  It has built-in
functionality to display information about the most commonly used services.")
    (license license:expat)))

(define-public wlclock
  (package
    (name "wlclock")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~leon_plickat/wlclock/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijicaizq9b4mkvpm9c5llmj5aqfyyasg7ll02py47yqf68khw38"))))
    (build-system meson-build-system)
    (inputs (list cairo wayland wayland-protocols))
    (native-inputs (list cmake-minimal pkg-config))
    (home-page "https://git.sr.ht/~leon_plickat/wlclock/")
    (synopsis "Digital analog clock for Wayland desktops")
    (description "Wlclock is a digital analog clock for Wayland desktops.
wlclock is inspired by xclock and the default configuration has been chosen to
mimic it.  However unlike xclock, wlclock is not a regular window but a
desktop-widget.  A Wayland compositor must implement the Layer-Shell and
XDG-Output for wlclock to work.")
    (license license:gpl3)))

(define-public wlroots
  (package
    (name "wlroots")
    (version "0.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l1c4m8m1h8rl00y9yi6qjma5m3lhai9hqv5578q69yg2dcwraxw"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'hardcode-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "xwayland/server.c"
                     (("Xwayland")
                      (search-input-file inputs "bin/Xwayland")))))
               (add-before 'configure 'fix-meson-file
                 (lambda* (#:key native-inputs inputs #:allow-other-keys)
                   (substitute* "backend/drm/meson.build"
                     (("/usr/share/hwdata/pnp.ids")
                      (search-input-file
                       (or native-inputs inputs) "share/hwdata/pnp.ids"))))))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
           eudev
           libdisplay-info
           libinput-minimal
           libxkbcommon
           mesa
           pixman
           lcms
           libseat
           vulkan-headers
           vulkan-loader
           wayland
           wayland-protocols
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (native-inputs
     (cons*
       glslang
       hwdata
       pkg-config
       wayland
       (if (%current-target-system)
         (list pkg-config-for-build)
         '())))
    (home-page "https://gitlab.freedesktop.org/wlroots/wlroots/")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))  ; MIT license

(define-public wlroots-0.17
  (package
    (inherit wlroots)
    (version "0.17.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name "wlroots" version))
       (sha256
        (base32 "0niigjpy8xxrnw3v9b3bsksw2q3yy3qsa2xx0aazwpycw5zrff83"))))))

(define-public wlroots-0.15
  (package
    (inherit wlroots)
    (name "wlroots-0.15")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name "wlroots" version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs wlroots)
                         (delete libdisplay-info)))))


(define-public wl-mirror
  (package
    (name "wl-mirror")
    (version "0.18.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ferdi265/wl-mirror")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n7i9jmij5vpnsas3j1namdrsncpp6q008nzny5kgbg96cq267ym"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ;No tests.
           #:configure-flags
           #~(list "-DINSTALL_DOCUMENTATION=ON"
                   "-DINSTALL_EXAMPLE_SCRIPTS=ON"
                   (string-append "-DWL_PROTOCOL_DIR="
                                  #$(this-package-input "wayland-protocols")
                                  "/share/wayland-protocols")
                   (string-append "-DWLR_PROTOCOL_DIR="
                                  #$(this-package-input "wlr-protocols")
                                  "/share/wlr-protocols"))))
    (inputs (list egl-wayland mesa wayland wayland-protocols wlr-protocols))
    (native-inputs (list pkg-config scdoc))
    (home-page "https://github.com/Ferdi265/wl-mirror")
    (synopsis "Simple Wayland output mirror client")
    (description
     "This package provides @command{wl-mirror}, a solution to @code{sway}'s
lack of output mirroring by mirroring an output onto a client surface.  It has
the following features:

@itemize
@item Mirror an output onto a resizable window.
@item Mirror an output onto another output by fullscreening the window.
@item React to changes in output scale (including fractional scaling).
@item Preserve aspect ratio.
@item Correct flipped or rotated outputs.
@item Custom flips or rotations.
@item Mirror custom regions of outputs.
@item Receive additional options on stdin for changing the mirrored screen or
region on the fly.
@end itemize")
    (license license:gpl3)))

(define-public wmenu
  (package
    (name "wmenu")
    (version "0.1.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/adnano/wmenu")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f46v4zbywh7fsz5dgkhaa62lmv7gydybwr7qym37gg10jz42pjc"))))
    (build-system meson-build-system)
    (native-inputs (append (if (%current-target-system)
                               ;; for wayland-scanner
                               (list pkg-config-for-build
                                     wayland)
                               '())
                           (list pkg-config scdoc)))
    (inputs (list cairo pango wayland libxkbcommon wayland-protocols))
    (home-page "https://codeberg.org/adnano/wmenu")
    (synopsis "Dynamic menu for Wayland")
    (description "@command{wmenu} is a dynamic menu for Wayland, which reads a list
of newline-separated items from stdin.  When the user selects an item and presses
Return, their choice is printed to stdout and wmenu terminates.  Entering text will
narrow the items to those matching the tokens in the input.")
    (license license:expat)))

(define-public sway
  (package
    (name "sway")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "175px3446mkm8015dhs1c4ia4a275hyfpvr6jnyghnx1rf9m06xq"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; elogind is propagated by wlroots -> libseat
      ;; and would otherwise shadow basu.
      #:configure-flags
      #~'("-Dsd-bus-provider=basu")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'hardcode-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Hardcode path to swaybg.
              (substitute* "sway/config.c"
                (("strdup..swaybg..")
                 (format #f "strdup(\"~a\")"
                         (search-input-file inputs "bin/swaybg")))))))))
    (inputs (list basu
                  cairo
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput-minimal
                  libxkbcommon
                  pango
                  pcre2
                  swaybg
                  wayland
                  wlroots))
    (native-inputs
     (cons* linux-pam mesa pkg-config scdoc wayland-protocols
            (if (%current-target-system)
              (list pkg-config-for-build
                    wayland)
              '())))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Wayland compositor compatible with i3")
    (description "Sway is a i3-compatible Wayland compositor.")
    (license license:expat)))

(define-public swayfx
  (package
    (inherit sway)
    (name "swayfx")
    (version "0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WillPower3309/swayfx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pvha915hihip59g6dkhjifm9rvwrvgxd3shc6xz99r377prpml1"))))
    (build-system meson-build-system)
    (inputs (list basu
                  cairo
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput-minimal
                  libxkbcommon
                  pango
                  pcre2
                  scenefx
                  swaybg
                  wayland
                  wlroots))
    (home-page "https://github.com/WillPower3309/swayfx")
    (synopsis "Sway Fork with extra options and effects")
    (description
     "Fork of Sway, a Wayland compositor compatible with i3.  SwayFX
adds extra options and effects to the original Sway, such as blur, rounded
corners, shadows, inactive window dimming, etc.")
    (license license:expat)))

(define-public swayidle
  (package
    (name "swayidle")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swayidle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y0qdqzx90kvk6l80darldvizr7p5g65bnblhxlq5a2rgvs9hkpx"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlogind-provider=elogind")))
    (inputs (list elogind wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/swayidle")
    (synopsis "Idle management daemon for Wayland compositors")
    (description "Swayidle is a idle management daemon for Wayland compositors.")
    (license license:expat))) ; MIT license

(define-public swaylock
  (package
    (name "swaylock")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaylock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n4m5nk2jj4f0p11760zdd51ncsb469d06hm0f5487v01p3igq6p"))))
    (build-system meson-build-system)
    (inputs (append (if (%current-target-system)
                        (list wayland-protocols)
                        '())
                    (list cairo gdk-pixbuf libxkbcommon linux-pam wayland)))
    (native-inputs (append (if (%current-target-system)
                               (list pkg-config-for-build wayland)
                               '())
                           (list pango pkg-config scdoc wayland-protocols)))
    (home-page "https://github.com/swaywm/swaylock")
    (synopsis "Screen locking utility for Wayland compositors")
    (description "Swaylock is a screen locking utility for Wayland compositors.")
    (license license:expat))) ; MIT license

(define-public swaylock-effects
  (package
    (inherit swaylock)
    (name "swaylock-effects")
    (version "1.7.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirutka/swaylock-effects")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cgpbzdpxj6bbpa8jwql1snghj21mhryyvj6sk46g66lqvwlrqbj"))))
    (arguments
     (list #:configure-flags #~'("-Dsse=false")))
    (synopsis "Screen locking utility for Wayland compositors with effects")
    (description "@code{Swaylock-effects} is a fork of swaylock with additional
features, such as the ability to take a screenshot as the background image,
display a clock or apply image manipulation techniques to the background image.")
    (home-page "https://github.com/jirutka/swaylock-effects")))

(define-public swaybg
  (package
    (name "swaybg")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaybg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "164jzs3sw08x92890bcs0sbfa3vs7l3n08fnrc1zzba42940z5r0"))))
    (build-system meson-build-system)
    (inputs
     (cons* cairo gdk-pixbuf wayland
            (if (%current-target-system)
              (list wayland-protocols)
              '())))
    (native-inputs
     (cons* pkg-config scdoc wayland-protocols
            (if (%current-target-system)
              (list pkg-config-for-build wayland)
              '())))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Screen wallpaper utility for Wayland compositors")
    (description "Swaybg is a wallpaper utility for Wayland compositors.")
    (license license:expat))) ; MIT license

(define-public swww
  (package
    (name "swww")
    (version "0.8.2")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/LGFae/swww")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ps10dv6a8a0hiw7p8kg64mf81pvavskmyn5xpbfw6hrc991vdlz"))
                (modules '((guix build utils)))
                (snippet
                 '(begin (substitute* "utils/Cargo.toml"
                           (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                            (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs
      `(("rust-log" ,rust-log-0.4)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-wayland-client" ,rust-wayland-client-0.31)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.18)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-keyframe" ,rust-keyframe-1)
        ("rust-rkyv" ,rust-rkyv-0.7)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-spin-sleep" ,rust-spin-sleep-1)
        ("rust-sd-notify" ,rust-sd-notify-0.4)
        ("rust-image" ,rust-image-0.24)
        ("rust-fast-image-resize" ,rust-fast-image-resize-2)
        ("rust-clap" ,rust-clap-4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-lzzzz" ,rust-lzzzz-1))
      #:cargo-development-inputs
      `(("rust-rand" ,rust-rand-0.8)
        ("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-criterion" ,rust-criterion-0.5))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'build-documentation
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "doc/gen.sh")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (share (string-append out "/share"))
                     (man1 (string-append share "/man/man1"))
                     (swww (car (find-files "target" "^swww$")))
                     (swww-daemon (car (find-files "target" "^swww-daemon$")))
                     (bash-completions-dir
                      (string-append share "/bash-completion/completions"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d"))
                     (elvish-completions-dir
                      (string-append share "/elvish/lib")))
                (install-file swww bin)
                (install-file swww-daemon bin)
                (copy-recursively "doc/generated" man1)
                (install-file "completions/swww.bash" bash-completions-dir)
                (install-file "completions/_swww" zsh-completions-dir)
                (install-file "completions/swww.fish" fish-completions-dir)
                (install-file "completions/swww.elv" elvish-completions-dir))))
          (add-after 'install 'wrap-binaries
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (lz4 (assoc-ref inputs "lz4")))
               (wrap-program (string-append out "/bin/swww")
                 `("PATH" prefix (,(string-append lz4 "/bin"))))
               (wrap-program (string-append out "/bin/swww-daemon")
                 `("PATH" prefix (,(string-append lz4 "/bin"))))))))))
    (native-inputs (list scdoc))
    (inputs (list bash-minimal lz4))
    (home-page "https://github.com/LGFae/swww")
    (synopsis
     "Efficient animated wallpaper daemon for wayland controlled at runtime")
    (description
     "A Solution to your Wayland Wallpaper Woes (swww).  It uses minimal resources
and provides animations for switching between backgrounds.")
    (license license:gpl3+)))

(define-public swaynotificationcenter
  (package
    (name "swaynotificationcenter")
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ErikReider/SwayNotificationCenter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0cx3ql7qb2wajck1vip9sm2a122jv9x8g2r0bnw4rrxd91yca7a9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list
                           "-Dsystemd-service=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-swaync-path
            (lambda _
              (substitute* "src/config.json.in"
                (("@JSONPATH@")
                 (string-append "\"" #$output
                                "/etc/xdg/swaync/configSchema.json\"")))
              (substitute* "src/functions.vala"
                (("/usr/local/etc/xdg/swaync")
                 (string-append #$output "/etc/xdg/swaync"))))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           pkg-config
           python-minimal
           sassc
           scdoc
           vala))
    (inputs
     (list json-glib
           glib
           granite-6
           gtk+
           gtk-layer-shell
           libhandy
           libgee
           pulseaudio
           wayland-protocols))
    (synopsis "Notification daemon with a graphical interface")
    (description
     "This package provides a notification daemon for the Sway Wayland
compository, supporting the following features:

@itemize
@item Keyboard shortcuts
@item Notification body markup with image support
@item A panel to view previous notifications
@item Show album art for notifications like Spotify
@item Do not disturb
@item Click notification to execute default action
@item Show alternative notification actions
@item Customization through a CSS file
@item Trackpad/mouse gesture to close notification
@item The same features as any other basic notification daemon
@item Basic configuration through a JSON config file
@item Hot-reload config through swaync-client
@end itemize")
    (home-page "https://github.com/ErikReider/SwayNotificationCenter")
    (license license:expat)))

(define-public waybar
  (package
    (name "waybar")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Alexays/Waybar")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i9an3yxbsbgpkl4zvwmk2g6vaap8shxix5gid6vx8x6z9wgg52n"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "--wrap-mode=nodownload")))
    (inputs (list date
                  fmt
                  gtk-layer-shell
                  gtkmm-3
                  jsoncpp
                  libdbusmenu
                  libevdev
                  libinput-minimal
                  libmpdclient
                  libnl
                  libxml2
                  pipewire
                  playerctl
                  pulseaudio
                  spdlog-1.13
                  wayland
                  wireplumber))
    (native-inputs
     (list `(,glib "bin") pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/Alexays/Waybar")
    (synopsis "Wayland bar for Sway and Wlroots based compositors")
    (description "Waybar is a highly customisable Wayland bar for Sway and
Wlroots based compositors.")
    (license license:expat))) ; MIT license

(define-public waybar-cpu-histogram
  (package
    (name "waybar-cpu-histogram")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~plattfot/waybar-cpu-histogram")
             (commit (string-append version))))
       (sha256
        (base32
         "001pyf1jdmf2478plnggd7dkfi688qwi89db2jwfp4zza3640na6"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list jsoncpp
           fmt))
    (synopsis "CPU histogram for waybar")
    (description
     "Custom module for waybar to show CPU usage as a histogram.  A compact way
to see how many cores are active, compared to having a bar for each
core/thread.")
    (home-page "https://github.com/plattfot/cpu-histogram/")
    (license license:expat)))

(define-public waybar-experimental
  (let ((base waybar))
    (package/inherit base
      (name "waybar-experimental")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags '())
          #~(cons "-Dexperimental=true"
                  #$flags))))
      (synopsis "Waybar with experimental features"))))

(define-public wlr-randr
  (package
    (name "wlr-randr")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/wlr-randr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kl5awwclmlfcxfz0jp8gzpg2vffsl9chfilysfsv1mq11a96ifs"))))
    (build-system meson-build-system)
    (inputs (list wayland))
    (native-inputs (list pkg-config))
    (home-page "https://sr.ht/~emersion/wlr-randr")
    (synopsis "Utility to manage Wayland compositor outputs")
    (description "wlr-randr is a utility to manage outputs of a Wayland compositor.")
    (license license:expat))) ; MIT license

(define-public mako
  (package
    (name "mako")
    (version "1.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/mako")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hv083k3gp1gl2gxi91f2xf21hcn33z68j6r5844hzi7g8wwmp9v"))))
    (build-system meson-build-system)
    (inputs (list basu cairo gdk-pixbuf pango wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://wayland.emersion.fr/mako")
    (synopsis "Lightweight Wayland notification daemon")
    (description "Mako is a lightweight notification daemon for Wayland
compositors that support the layer-shell protocol.")
    (license license:expat))) ; MIT license

(define-public kanshi
  (package
    (name "kanshi")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/kanshi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g5glpkcn54ypfym4lpfdjai479yfazcai1rg86bn72nkcbpwfql"))))
    (build-system meson-build-system)
    (inputs (list libscfg libvarlink wayland))
    (native-inputs (append (if (%current-target-system)
                               (list pkg-config-for-build)
                               (list))
                           (list pkg-config scdoc wayland)))
    (home-page "https://wayland.emersion.fr/kanshi")
    (synopsis "Hotswappable output profiles for Wayland")
    (description "Kanshi allows you to define output profiles that are
automatically enabled and disabled on hotplug.  Kanshi can be used with
Wayland compositors supporting the wlr-output-management protocol.")
    (license license:expat))) ; MIT license

(define-public wdisplays
  (package
    (name "wdisplays")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artizirk/wdisplays.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ydcmfdics2qqjb14p767xs8khd86nancdd9z8j11h2gpvwznvn"))))
    (build-system meson-build-system)
    (inputs (list gtk+ libepoxy wayland))
    (native-inputs (list `(,glib "bin") pkg-config))
    (home-page "https://github.com/artizirk/wdisplays")
    (synopsis "Configuring displays in Wayland compositors")
    (description "@command{wdisplays} is a graphical application for
configuring displays in Wayland compositors that implements the
wlr-output-management-unstable-v1 protocol.")
    (license license:gpl3+)))

(define-public stumpwm
  (package
    (name "stumpwm")
    (version "24.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stumpwm/stumpwm")
             (commit version)))
       (file-name (git-file-name "stumpwm" version))
       (sha256
        (base32 "0b8h33raf0ffl2zv678sxqpvq5xhy6sa88sdm7krnwcd15q8gb85"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiasco
           texinfo

           ;; To build the manual.
           autoconf
           automake))
    (inputs
     (list sbcl-alexandria
           sbcl-cl-ppcre
           sbcl-clx))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append #$output "/bin/stumpwm")
               outputs
               #:entry-program '((stumpwm:stumpwm) 0))))
          (add-after 'build-program 'create-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out #$output)
                     (xsessions (string-append out "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/stumpwm.desktop")
                  (lambda (file)
                    (format file
                       "[Desktop Entry]~@
                        Name=stumpwm~@
                        Comment=The Stump Window Manager~@
                        Exec=~a/bin/stumpwm~@
                        TryExec=~@*~a/bin/stumpwm~@
                        Icon=~@
                        Type=Application~%"
                       out))))))
          (add-after 'create-desktop-file 'install-manual
            (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
              (let* ((out  #$output)
                     (info (string-append out "/share/info")))
                (invoke "./autogen.sh")
                (invoke "sh" "./configure" "SHELL=sh")
                (apply invoke "make" "stumpwm.info" make-flags)
                (install-file "stumpwm.info" info)))))))
    (synopsis "Window manager written in Common Lisp")
    (description
     "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
    (home-page "https://github.com/stumpwm/stumpwm")
    (license license:gpl2+)
    (properties `((cl-source-variant . ,(delay cl-stumpwm))))))

(define-public cl-stumpwm
  (package
    (inherit (sbcl-package->cl-source-package stumpwm))
    (name "cl-stumpwm")))

(define-public stumpwm+slynk
  (package
    (inherit stumpwm)
    (name "stumpwm-with-slynk")
    (inputs
     (list sbcl-slynk stumpwm))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (setenv "HOME" "/tmp")
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm" "slynk")
                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm" "sbcl-slynk"))))))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'remove-temporary-cache)
           (delete 'cleanup)))))))

(define stumpwm-contrib
  (let ((commit "c4f077b1fe97cd8da6d710e5cbe390eb680629bd")
        (revision "7"))
    (package
      (name "stumpwm-contrib")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0fdv4d0rlca64p4dakp1l60701vls2s6kx3gzlflmcf2l49kdbnn"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list stumpwm))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "StumpWM extra modules")
      (description "This package provides extra modules for StumpWM.")
      (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))

(define-public stumpish
  (package
    (inherit stumpwm-contrib)
    (name "stumpish")
    (inputs
     (list bash rlwrap xprop))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (chdir "util/stumpish")
         (substitute* "stumpish"
           (("rlwrap") (search-input-file %build-inputs "/bin/rlwrap"))
           (("xprop") (search-input-file %build-inputs "/bin/xprop"))
           (("/bin/sh") (search-input-file %build-inputs "/bin/bash")))
         (install-file "stumpish" (string-append %output "/bin")))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "StumpWM interactive shell")
    (description "This package provides a StumpWM interactive shell.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-pamixer
  (let ((commit "aa820533c80ea1af5a0e107cea25eaf34e69dc24")
        (revision "1"))
    (package
      (name "sbcl-stumpwm-pamixer")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Junker/stumpwm-pamixer")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0djcrr16bx40l7b60d4j507vk5l42fdgmjpgrnk86z1ba8wlqim8"))))
      (inputs (list pamixer stumpwm))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list #:asd-systems ''("pamixer")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-pamixer
                   (lambda _
                     (substitute* "pamixer.lisp"
                       (("\"pamixer \"")
                        (string-append "\""
                                       #$(this-package-input "pamixer")
                                       "/bin/pamixer \""))))))))
      (home-page "https://github.com/Junker/stumpwm-pamixer")
      (synopsis "StumpWM Pamixer Module")
      (description
       "This package provides a minimalistic Pulseaudio volume and microphone
control module for StumpWM.")
      (license license:gpl3))))

(define-public sbcl-stumpwm-binwarp
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-binwarp")
    (arguments
     '(#:asd-systems '("binwarp")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/binwarp"))))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Keyboard-driven divide-and-conquer mouse control mode")
    (description "This package provides a keyboard-driven divide-and-conquer
mouse control mode for StumpWM.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-stump-nm
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-stump-nm")
    (arguments
     '(#:asd-systems '("stump-nm")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/stump-nm"))))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (inputs (list stumpwm
                  sbcl-babel
                  sbcl-alexandria
                  sbcl-dbus))
    (synopsis "StumpWM NetworkManager integration")
    (description "This module allows you to manage your Wi-Fi networks and VPN
connections from within StumpWM itself.  It is intentionally pretty bare-bones in
features, in that it allows you to enable/disable connections, and no more.  It is
not a replacement for nmtui and/or nmcli.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-tomato
  (package
    (name "sbcl-stumpwm-tomato")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Junker/stumpwm-tomato")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qq11spvhrcq74gh0dw0p4859ai2mqzbxa45gjbf599kmcybp1pa"))))
    (build-system asdf-build-system/sbcl)
    (inputs (list stumpwm))
    (arguments
     '(#:asd-systems '("tomato")
       #:tests? #f))
    (home-page "https://github.com/Junker/stumpwm-tomato")
    (synopsis "Advanced Pomodoro timer module for StumpWM")
    (description
     "This package provides an advanced Pomodoro timer module for StumpWM.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-ttf-fonts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-ttf-fonts")
    (inputs
     (list sbcl-clx-truetype stumpwm))
    (arguments
     '(#:asd-systems '("ttf-fonts")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/ttf-fonts") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Implementation of TTF font rendering for Lisp")
    (description "This package provides a Lisp implementation of TTF font
rendering.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-pass
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-pass")
    (arguments
     '(#:asd-systems '("pass")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/pass") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Integrate @code{pass} with StumpWM")
    (description "This package provides an interface which integrates
password-store into StumpWM.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-rofi
  (package
    (name "sbcl-stumpwm-rofi")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Junker/stumpwm-rofi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s0sri19rv6dshwm49djgbfc0zajl63gr1rg8xl762chlj28h1ir"))))
    (build-system asdf-build-system/sbcl)
    (inputs (list stumpwm rofi))
    (arguments
     '(#:asd-systems '("rofi")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rofi-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "rofi.lisp"
               (("rofi -dmenu")
                (string-append (search-input-file inputs "/bin/rofi")
                               " -dmenu"))))))))
    (home-page "https://github.com/Junker/stumpwm-rofi")
    (synopsis "Rofi module for StumpWM")
    (description "This package provides Rofi integration for StumpWM.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-globalwindows
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-globalwindows")
    (arguments
     '(#:asd-systems '("globalwindows")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/globalwindows") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Manipulate all windows in the current X session")
    (description "This package provides a StumpWM module to manipulate all
windows in the current X session.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-swm-gaps
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-swm-gaps")
    (arguments
     '(#:asd-systems '("swm-gaps")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/swm-gaps") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Gaps between windows for StumpWM")
    (description "This package provides a StumpWM module which adds gaps
between windows.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-net
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-net")
    (arguments
     '(#:asd-systems '("net")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/net") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/net")
    (synopsis "Modeline support for network connectivity")
    (description "Modeline support for network connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-wifi
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-wifi")
    (arguments
     '(#:asd-systems '("wifi")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/wifi") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/wifi")
    (synopsis "Modeline support for wifi connectivity")
    (description "Modeline support for wifi connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-stumptray
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-stumptray")
    (arguments
     '(#:asd-systems '("stumptray")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/stumptray") #t)))))
    (inputs
     (list sbcl-alexandria sbcl-clx-xembed stumpwm))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/stumptray")
    (synopsis "Modeline support for stumptray connectivity")
    (description "Modeline support for stumptray connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-kbd-layouts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-kbd-layouts")
    (arguments
     '(#:asd-systems '("kbd-layouts")
       #:asd-operation "compile-system"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "util/kbd-layouts") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/kbd-layouts")
    (synopsis "Keyboard layout switcher for StumpWM")
    (description "Keyboard layout switcher for StumpWM")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-numpad-layouts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-numpad-layouts")
    (arguments
     '(#:asd-systems '("numpad-layouts")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/numpad-layouts"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/numpad-layouts")
    (synopsis "Keyboard numpad layouts for StumpWM")
    (description "This is a module for handling different keyboards numpad
layouts in StumpWM.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-cpu
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-cpu")
    (arguments
     '(#:asd-systems '("cpu")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/cpu"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu")
    (synopsis "Modeline support for CPU info")
    (description "Modeline support for CPU info.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-disk
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-disk")
    (arguments
     '(#:asd-systems '("disk")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modeline/disk") #t)))))
    (inputs
     (list sbcl-cl-diskspace sbcl-cl-mount-info stumpwm))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "StumpWM modeline support to show disk usage")
    (description "StumpWM modeline support to show disk usage")
    (license (list license:gpl2+ license:gpl3+))))

(define-public sbcl-stumpwm-mem
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-mem")
    (arguments
     '(#:asd-systems '("mem")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/mem"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem")
    (synopsis "Modeline support for memory info")
    (description "Modeline support for memory info.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-winner-mode
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-winner-mode")
    (arguments
     '(#:asd-systems '("winner-mode")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/winner-mode"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/winner-mode")
    (synopsis "Emacs' winner-mode for StumpWM")
    (description "This module provides a winner-mode for StumpWM similar to the
one in Emacs.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-screenshot
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-screenshot")
    (inputs
     (list sbcl-zpng stumpwm))
    (arguments
     '(#:asd-systems '("screenshot")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/screenshot")))
         (add-after 'chdir 'fix-build
           (lambda _
             (substitute* "screenshot.asd"
               (("#:zpng")
                "#:stumpwm #:zpng")))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/screenshot")
    (synopsis "Screenshots for StumpWM")
    (description "This StumpWM module can take screenshots and store them as
PNG files.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-hostname
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-hostname")
    (arguments
     '(#:asd-systems '("hostname")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/hostname"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/hostname")
    (synopsis "Put hostname in the StumpWM modeline")
    (description "This StumpWM module puts the hostname in the StumpWM
modeline.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-notify
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-notify")
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-bordeaux-threads
           sbcl-dbus
           sbcl-xml-emitter
           stumpwm))
    (arguments
     '(#:asd-systems '("notify")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/notify"))))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Notifications server for StumpWM")
    (description "This module implements org.freedesktop.Notifications
interface[fn:dbus-spec].  It shows notifications using stumpwm:message
by default.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-battery-portable
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-battery-portable")
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-cl-ppcre stumpwm))
    (arguments
     '(#:asd-systems '("battery-portable")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modeline/battery-portable"))))))
    (synopsis "Battery level indicator for StumpWM")
    (description "This module provides a battery level indicator for the
modeline.  It can be displayed in the modeline with %B.")
    (license (list license:expat license:gpl3+))))

(define-public lemonbar
  (package
    (name "lemonbar")
    (version "1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LemonBoy/bar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sm1lxxf0y2n87nvc8mz6i6mzb32f4qab80ppb28ibrwfir6jsln"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:make-flags
      #~(list #$(string-append "CC=" (cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (inputs
     (list libxcb))
    (native-inputs
     (list perl))
    (home-page "https://github.com/LemonBoy/bar")
    (synopsis "Featherweight status bar")
    (description
     "@code{lemonbar} (formerly known as @code{bar}) is a lightweight
bar entirely based on XCB.  Provides full UTF-8 support, basic
formatting, RandR and Xinerama support and EWMH compliance without
wasting your precious memory.")
    (license license:x11)))

(define-public lemonbar-xft
  ;; Upstream v2.0 tag is several years behind HEAD
  (let ((commit "481e12363e2a0fe0ddd2176a8e003392be90ed02"))
    (package
      (inherit lemonbar)
      (name "lemonbar-xft")
      (version (string-append "2.0." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/drscream/lemonbar-xft")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0588g37h18lv50h7w8vfbwhvc3iajh7sdr53848spaif99nh3mh4"))))
      (inputs
       (modify-inputs (package-inputs lemonbar)
         (prepend freetype libxft libx11)))
      (arguments
       (substitute-keyword-arguments (package-arguments lemonbar)
         ((#:make-flags make-flags)
          #~(#$@make-flags
             (format #f "CFLAGS=~a -DVERSION='~s'"
                     (string-append
                      "-I" #$(this-package-input "freetype")
                      "/include/freetype2")
                     #$version)))))
      (home-page "https://github.com/drscream/lemonbar-xft")
      (synopsis
       (string-append
        (package-synopsis lemonbar)
        " with fontconfig support"))
      (description
       (string-append
        (package-description lemonbar)
        "This is a fork of the @code{lemonbar} package that adds fontconfig
support, for easier unicode usage.")))))

(define-public xclickroot
  (package
    (name "xclickroot")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xclickroot")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wnsfxvh4v02r2jjyh2n6jfkbj2dna2hlm6anl4b36lwji749k2k"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xclickroot")
    (synopsis "Run a command when a mouse button is pressed on the root window")
    (description "@code{xclickroot} runs a command every time a given mouse
button is pressed on the root window.")
    (license license:public-domain)))

(define-public xinitrc-xsession
  (let ((commit "cbfc77a1ccaf07b7d8a35f4d8007c7102f365374")
        (revision "0"))
    (package
      (name "xinitrc-xsession")
      (version (git-version "1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://aur.archlinux.org/xinit-xsession.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12nv3qyjhy2l9mcb547f414d8bj79mhdhsra0g8x7x71b1xxl15b"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'patch-xsession-file
              (lambda _
                (let* ((xinitrc-desktop
                        (string-append #$output "/share/xsessions/xinitrc.desktop"))
                       (xinitrc-helper
                        (string-append #$output "/bin/xinitrcsession-helper")))
                  (substitute* xinitrc-desktop
                    (("Exec=xinitrcsession-helper")
                     (string-append "Exec=" xinitrc-helper)))))))
        #:install-plan
        #~(list '("xinitrcsession-helper" "bin/")
                '("xinitrc.desktop" "share/xsessions/"))))
      (home-page "https://aur.archlinux.org/packages/xinit-xsession/")
      (synopsis "Use ~/.xinitrc as an xsession from your display manager")
      (description
       "Xinitrc-xsession allows @code{~/.xinitrc} to be run as a session from
your display manager.  Make @code{~/.xinitrc} executable and use this package
in your system configuration have this xsession available to your display
manager.")
      (license license:gpl3))))

(define-public xmenu
  (package
    (name "xmenu")
    (version "4.5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xmenu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qgxkrv9jnnnf3px7zh0paf8xsr4bcpf0f2nq9zy012m214223hs"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxinerama libxft freetype imlib2))
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output)
             (string-append "CFLAGS="
                            "-I" (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xmenu")
    (synopsis "Menu utility for X")
    (description "@code{xmenu} receives a menu specification in stdin, shows
a menu for the user to select one of the options, and outputs the option
selected to stdout.  It can be controlled both via mouse and via keyboard.")
    (license license:public-domain)))

(define-public idesk
  (package
    (name "idesk")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/idesk/idesk/idesk-" version
             "/idesk-" version ".tar.bz2"))
       (sha256
        (base32
         "1lxk2yvgysxwl514zc82lwr1dwc8cd62slgr5lzdhjbdrxfymdyl"))
       (modules '((guix build utils)
                  (ice-9 format)))
       (snippet
        '(let* ((file     "src/DesktopConfig.cpp")
                (template (string-append file ".XXXXXX"))
                (out      (mkstemp! template))
                (st       (stat file))
                (mode     (stat:mode st)))
           (call-with-ascii-input-file file
             (lambda (p)
               (format out "~{~a~%~}" '("#include <unistd.h>"
                                        "#include <sys/stat.h>"
                                        "#include <sys/types.h>"))
               (dump-port p out)
               (close out)
               (chmod template mode)
               (rename-file template file)
               #t))))))
    (build-system gnu-build-system)
    (inputs
     (list libx11
           libxft
           libxpm
           libpng
           freetype
           imlib2-1.7
           sed))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:tests? #f)) ;no test suite
    (home-page "https://sourceforge.net/projects/idesk/")
    (synopsis "Add icons on X desktop and set background image for wallpaper")
    (description "Idesk is program that draws desktop icons.  Each icon will
execute a shell command on a configurable action.  The icons can be moved on
the desktop by dragging them, and the icons will remember their positions on
start-up.")
    (license license:bsd-3)))

(define-public xnotify
  (package
    (name "xnotify")
    (version "0.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xnotify")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jxms4md2mwfjgm2pgg3vakpp33800jbn9hnl0j4jyfc9f1ckbsv"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxft libxinerama imlib2))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output)
             (string-append "CFLAGS="
                            "-I" (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))
       #:tests? #f ;no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xnotify")
    (synopsis "Displays a notification on the screen")
    (description "XNotify receives a notification specification in stdin and
shows a notification for the user on the screen.")
    (license license:expat)))

(define-public cagebreak
  (package
    (name "cagebreak")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/project-repo/cagebreak")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0firjpp7qw4kb2h1zh5pv5k0xf0jvx6x0r0s7j6y7dhlh5j0s00q"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dxwayland=true" "-Dman-pages=true")
      ;; XXX: Running cagebreak tests need more tools, such as: clang-format,
      ;; shellcheck, git, gnupg ...
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-data-dir
            (lambda _
              (substitute* '("cagebreak.c" "meson.build")
                (("/etc/") (string-append #$output "/etc/"))
                (("/usr/share/") (string-append #$output "/usr/share/"))))))))
    (native-inputs (list pkg-config scdoc))
    (inputs (list libevdev pango wlroots-0.17))
    (home-page "https://github.com/project-repo/cagebreak")
    (synopsis "Tiling wayland compositor inspired by ratpoison")
    (description
     "@command{cagebreak} is a slim, keyboard-controlled, tiling compositor
for wayland conceptually based on the X11 window manager
@command{ratpoison}.")
    (license license:expat)))

(define-public libdisplay-info
  (let ((commit "ebee35935dad01478ae1ae5ead298c4cd8018ac2")
        (revision "0"))
    (package
      (name "libdisplay-info")
      (version (git-version "0.2.0-dev" revision commit))
      (home-page "https://gitlab.freedesktop.org/emersion/libdisplay-info")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ly8acdjxn8l55y0wc07n7pb6rzh9dpr1vbsakdib2zrl0i5yh3a"))))
      (build-system meson-build-system)
      (arguments
       (list
        #:phases #~(modify-phases %standard-phases
                     (add-before 'configure 'fix-meson-file
                       (lambda* (#:key native-inputs inputs #:allow-other-keys)
                         (substitute* "meson.build"
                           (("/usr/share/hwdata/pnp.ids")
                            (string-append (assoc-ref (or native-inputs inputs)
                                                      "hwdata")
                                           "/share/hwdata/pnp.ids"))))))))
      (native-inputs (list hwdata python))
      (synopsis "EDID and DisplayID library")
      (description
       "This package provides a library to read @acronym{EDID, Extended
Display Identification Data} and DisplayID metadata from display devices.  It
has the following goals:

@enumerate
@item
Provide a set of high-level, easy-to-use, opinionated functions
as well as low-level functions to access detailed information.
@item
Simplicity and correctness over performance and resource usage.
@item
Well-tested and fuzzed.
@end enumerate")
      (license license:expat))))

(define-public libucl
  (package
    (name "libucl")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vstakhov/libucl/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j8npymjhcnzbwhx1wggr88148cga921438flf1sn7mw1b9dr55f"))))
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ;; no tests
    (home-page "https://github.com/vstakhov/libucl")
    (synopsis "Universal configuration language (UCL) parser")
     (description "libucl implements a configuration language that is easy to
read and write, and compatible with JSON.")
    (license license:bsd-2)))

(define-public labwc
  (package
    (name "labwc")
    (version "0.8.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/labwc/labwc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wbza00y2xf2zn34q5c8g5k2dn2xjzbbqmsnjv6c90mh2bbk1q95"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config gettext-minimal scdoc))
    (inputs
     (list cairo
           glib
           (librsvg-for-system)
           libliftoff
           libsfdo
           libxcb
           libxml2
           pango
           wlroots))
    (home-page "https://labwc.github.io")
    (synopsis "Window-stacking compositor for Wayland")
    (description
     "Labwc is lightweight and independent with a focus on simply stacking
windows well and rendering some window decorations, it is inspired by Openbox.
It takes a no-bling/frills approach and says no to features such as icons
(except window buttons), animations, decorative gradients and any other
options not required to reasonably render common themes.  It relies on clients
for panels, screenshots, wallpapers and so on to create a full desktop
environment.

Labwc tries to stay in keeping with wlroots and sway in terms of general
approach and coding style.

Labwc has no reliance on any particular Desktop Environment, Desktop Shell or
session.  Nor does it depend on any UI toolkits such as Qt or GTK.")
    (license license:gpl2)))

(define-public hikari
  (package
    (name "hikari")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hikari.acmelabs.space/releases/"
                           "hikari-" version ".tar.gz"))
       (sha256
        (base32 "1qjd9dhpmv75idf6jjzwff8wgliad4d0af8pih9526p14vrnyws0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bmake pkg-config wayland-protocols))
    (inputs
     (list cairo
           libinput-minimal
           libucl
           libxkbcommon
           linux-pam
           pango
           wayland
           wlroots-0.15))
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out"))
        (string-append "CC=" ,(cc-for-target))
        "WITH_XWAYLAND=YES"
        "WITH_SCREENCOPY=YES"
        "WITH_LAYERSHELL=YES"
        "WITH_VIRTUAL_INPUT=YES")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (apply invoke "bmake" make-flags)))
         (replace 'install
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (apply invoke "bmake" "install" make-flags))))))
    (home-page "https://hikari.acmelabs.space/")
    (synopsis "Stacking Wayland compositor with tiling capabilities")
    (description
     "Hikari is a stacking Wayland compositor with additional tiling
capabilities.  It is heavily inspired by the Calm Window manager(cwm).")
    (license license:bsd-2)))

(define-public jwm
  (package
    (name "jwm")
    (version "2.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/joewing/jwm/releases/download/"
                    "v" version "/jwm-" version ".tar.xz"))
              (sha256
               (base32
                "0bc0vnaz3pk8msrslpj5ii4iv4fc4iayv0rbl8zlnn8phg11x1xm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f   ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-xsession
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/jwm.desktop")
                  (lambda (port)
                    (format port "~
                     [Desktop Entry]~@
                     Name=jwm~@
                     Comment=Joe's Window Manager~@
                     Exec=~a/bin/jwm~@
                     Type=XSession~%" out)))))))))
    (native-inputs (list pkg-config))
    (inputs
     (list cairo
           libjpeg-turbo
           libpng
           (librsvg-for-system)
           libxext
           libxinerama
           libxmu
           libxpm
           libxrender
           libxt
           pango))
    (home-page "http://joewing.net/projects/jwm")
    (synopsis "Joe's Window Manager")
    (description
     "JWM is a light-weight window manager for the X11 Window System.  it is
written in C and uses only Xlib at a minimum.  Because of its small footprint,
it makes a good window manager for older computers and less powerful systems,
such as the Raspberry Pi, though it is perfectly capable of running on modern
systems.")
    (license license:expat)))

(define-public mjwm
  (package
    (name "mjwm")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chiku/mjwm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lgfp2xidhvmbj4zqvzz9g8zwbn6mz0pgacc57b43ha523vamsjq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f   ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-subcategory.h
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "include/subcategory.h"
                ;; icon name should be application-other instead of
                ;; application-others.
                (("applications-others") "applications-other")))))))
    (home-page "https://github.com/chiku/mjwm")
    (synopsis "Create menu for JWM")
    (description
     "MJWM can create JWM's menu from (freedesktop) desktop files and the
generated file can be include in the rootmenu section of your jwm config
file.")
    (license license:gpl2+)))

(define-public devour
  (package
    (name "devour")
    (version "12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/salman-abedin/devour")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qq5l6d0fn8azg7sj7a4m2jsmhlpswl5793clcxs1p34vy4wb2lp"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ;no configure script
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "BIN_DIR=" %output "/bin"))))
    (home-page "https://github.com/salman-abedin/devour")
    (synopsis "X11 window swallower")
    (description
     "@command{devour} hides your current window before launching an external
program and unhides it after quitting.")
    (license license:gpl2)))

(define-public trayer-srg
  (package
    (name "trayer-srg")
    (version "1.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sargon/trayer-srg")
             (commit (string-append "trayer-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1mvhwaqa9bng9wh3jg3b7y8gl7nprbydmhg963xg0r076jyzv0cg"))))
    (native-inputs
     (list libxmu pkg-config))
    (inputs
     (list libx11 gdk-pixbuf gtk+-2))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "CC=" ,(cc-for-target))
               (string-append "PREFIX=" %output)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (apply invoke "./configure" configure-flags))))))
    (home-page "https://github.com/sargon/trayer-srg")
    (synopsis "Minimal GTK based system tray")
    (description
     "@command{trayer} is small program designed to provide systray
functionality present in GNOME/KDE desktop environments for window managers
which do not support it.")
    (license license:expat)))

(define-public wlogout
  (package
    (name "wlogout")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ArtsyMacaw/wlogout")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pzgpfnfzpkc6y14x4g5wv5ldm67vshcp893i4rszfx4kv5ikmpy"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list gtk-layer-shell gtk+))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack  'patch-source-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (list "main.c" "style.css")
                 (("/usr/share") (string-append out "/share"))
                 (("/etc") (string-append out "/etc"))))
             #t)))))
    (home-page "https://github.com/ArtsyMacaw/wlogout")
    (synopsis "Logout menu for Wayland")
    (description "wlogout is a logout menu for Wayland environments.")
    (license license:expat)))

(define-public berry
  (package
    (name "berry")
    (version "0.1.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/jlervin/berry")
          (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ygqzgi7ncc6whvwdifds2cq9cllq9fhiqnigx859hbdnf453hn4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       ,#~(list (string-append "CC=" #$(cc-for-target))
                (string-append "prefix=" #$output)
                (string-append "CFLAGS="
                               "-I" (assoc-ref %build-inputs "freetype")
                               "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (xsessions (string-append output "/share/xsessions")))
               (mkdir-p xsessions)
               (with-output-to-file (string-append xsessions "/berry.desktop")
                 (lambda _
                   (format #t
                    "\
[Desktop Entry]~@
Name=berry~@
Comment=Berry Window Manager~@
Exec=~a/bin/berry~@
TryExec=~@*~a/bin/berry~@
Icon=~@
Type=Application~%"
                    output)))))))))
    (native-inputs
     (list pkg-config))
    (inputs
      (list freetype
            fontconfig
            libxext
            libx11
            libxft
            libxinerama))
    (home-page "https://berrywm.org/")
    (synopsis "Healthy, byte-sized window manager")
    (description
     "@code{berry} is a healthy, bite-sized window manager written in C using XLib.")
    (license license:expat)))

(define-public avizo
  (package
    (name "avizo")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/misterdanb/avizo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01v1c9376pbjndyhj9r6f214kzhivl1m9pkl05sdkcj0v6n0wgsn"))))
    (build-system meson-build-system)
    (inputs (list gtk+))
    (native-inputs
     (list vala
           `(,glib "bin")
           gobject-introspection
           gtk-layer-shell
           pkg-config))
    (home-page "https://github.com/misterdanb/avizo")
    (synopsis "Notification daemon for Sway")
    (description
     "Avizo is a simple notification daemon for Sway, mainly intended to be
used for multimedia keys.")
    (license license:gpl3+)))

(define-public grimblast
  (let ((commit "9d67858b437d4a1299be496d371b66fc0d3e01f6")
        (revision "1"))
    (package
      (name "grimblast")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/contrib")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1v0v5j7ingx80b5zpyz8ilfhz0kh9dcssnx6iwwl45zzgp915cpv"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                ;No tests.
             #:make-flags
             #~(list (string-append "PREFIX=" #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-after 'unpack 'chdir
                   (lambda _
                     (chdir "grimblast")))
                 (add-after 'chdir 'fix-paths
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "grimblast"
                       (((string-append "\\<(" (string-join
                                                '("date"
                                                  "grim"
                                                  "slurp"
                                                  "hyprctl"
                                                  "hyprpicker"
                                                  "wl-copy"
                                                  "jq"
                                                  "notify-send")
                                                "|")
                                        ")\\>")
                         cmd)
                        (search-input-file
                         inputs (string-append "bin/" cmd)))))))))
      (native-inputs (list scdoc))
      (inputs
       (list coreutils-minimal
             grim
             jq
             libnotify
             slurp
             hyprland
             hyprpicker
             wl-clipboard))
      (home-page "https://github.com/hyprwm/contrib")
      (synopsis "Screenshot utility for Hyprland")
      (description
       "This package provides a Hyprland version of @code{grimshot} for
screenshoting.")
      (license license:expat))))

(define-public grimshot
  (package
    (name "grimshot")
    (version "1.9-contrib.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OctopusET/sway-contrib")
                    (commit version)))
              (file-name (git-file-name name version))
              (snippet #~(delete-file "grimshot.1"))
              (sha256
               (base32
                "16fa8l81zjy25nsky1i525hb7zjprqz74mbirm9b76pvksschdv5"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~`(("grimshot" "bin/")
                              ("grimshot.1" "share/man/man1/"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-script-dependencies
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "grimshot"
                              (("\\b(date|grim|jq|notify-send|slurp|swaymsg|wl-copy)\\b"
                                _ binary)
                               (search-input-file
                                inputs (string-append "bin/" binary))))))
                        (add-after 'patch-script-dependencies 'build-man-page
                          (lambda _
                            (with-input-from-file "grimshot.1.scd"
                              (lambda _
                                (with-output-to-file "grimshot.1"
                                  (lambda _
                                    (invoke "scdoc"))))))))))
    (native-inputs (list scdoc))
    (inputs (list coreutils
                  grim
                  jq
                  libnotify
                  slurp
                  sway
                  wl-clipboard))
    (home-page "https://github.com/OctopusET/sway-contrib")
    (synopsis "Screenshot utility for the Sway window manager")
    (description "Grimshot is a screenshot utility for @code{sway}.  It provides
an interface over @code{grim}, @code{slurp} and @code{jq}, and supports storing
the screenshot either directly to the clipboard using @code{wl-copy} or to a
file.")
    (license license:expat)))

(define-public wld
  (let ((commit "6586736176ef50a88025abae835e29a7ca980126")
        (revision "1"))
    (package
      (name "wld")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/wld")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qkd3q8p1s72x688g83fkcarrz2h20904rpd8z44ql6ksgrj9bp3"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                              ; no tests
         #:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list fontconfig
                    libdrm
                    pixman
                    wayland))
      (propagated-inputs (list fontconfig pixman))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/wld")
      (synopsis "Primitive drawing library for Wayland")
      (description "wld is a drawing library that targets Wayland.")
      (license license:expat))))

(define-public swc
  (let ((commit "a7b615567f83d9e48d585251015048c441ca0239")
        (revision "1"))
    (package
      (name "swc")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/swc")
                      (commit commit)))
                (sha256
                 (base32
                  "19rpbwpi81pm92fkhsmbx7pzagpah5m9ih5h5k3m8dy6r8ihdh35"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no tests
         #:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list libdrm
                    libinput
                    libxcb
                    libxkbcommon
                    wayland
                    wayland-protocols
                    wld
                    xcb-util-wm))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/swc")
      (synopsis "Library for making a simple Wayland compositor")
      (description
       "swc is a small Wayland compositor implemented as a library.

It has been designed primarily with tiling window managers in mind.  Additionally,
notable features include:
@itemize
@item Easy to follow code base
@item XWayland support
@item Can place borders around windows
@end itemize")
      (license license:expat))))

(define-public velox
  (let ((commit "fcc041265539befd907a64ee3a536cb2489ffb99")
        (revision "1"))
    (package
      (name "velox")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/velox")
                      (commit commit)))
                (sha256
                 (base32
                  "0d11bmag5zwmas3rf1b7x5hjla7wpxq70nr86dz3x9r7cal04mym"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no tests
         #:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list libinput libxkbcommon wayland wld))
      (propagated-inputs (list swc))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/velox")
      (synopsis "Simple window manager based on swc")
      (description "velox is a simple window manager for Wayland based on swc.
It is inspired by dwm and xmonad.")
      (license license:expat))))

(define-public wbg
  ;; This commit fixes a build error: https://codeberg.org/dnkl/wbg/issues/11
  (let ((commit "dd36cce8c47bb0e17a789cf2bd95a51e29b59e78")
        (revision "0"))
    (package
      (name "wbg")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/dnkl/wbg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0nsb8w3myprhnwr59i6g4nwkc8fx67d40l70svjmwfmhpqy6zc18"))))
      (build-system meson-build-system)
      (arguments
       (list
        #:build-type "release"
        #:configure-flags #~(list "-Dpng=enabled"
                                  "-Djpeg=enabled"
                                  "-Dwebp=enabled")))
      (native-inputs (list pkg-config tllist wayland-protocols))
      (inputs (list libjpeg-turbo libpng libwebp pixman wayland))
      (home-page "https://codeberg.org/dnkl/wbg")
      (synopsis "Wallpaper application for Wayland compositors")
      (description
       "wbg is a super simple wallpaper application for Wayland compositors
implementing the layer-shell protocol.")
      (license license:expat))))

(define-public wsbg
  (let ((commit "15b0d0f6910ea97b9bcc471695fac07270955dd2")
        (revision "0")
        ;; Upstream has no version tags, but meson.build contains the correct
        ;; version number.
        (version "0.1.0"))
    (package
      (inherit swaybg)
      (name "wsbg")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/saibier/wsbg")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15xrnwp54kghigx06y4qmmn1q9f26fx4cawyl4kcbqrrzfbbj1g6"))))
      (home-page "https://github.com/saibier/wsbg")
      (synopsis "Workspace wallpaper tool for Sway")
      (description "Wallpaper utility for Sway with support for per-workspace
configuration."))))

(define-public yambar-wayland
  (package
    (name "yambar-wayland")
    (version "1.11.0")
    (home-page "https://codeberg.org/dnkl/yambar")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c3sk2i14fcb0l95pvfnj2sx0vx4ql1vldhimfccbf2qj0r30b20"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:configure-flags #~'("-Db_lto=true"
                            "-Dbackend-x11=disabled"
                            "-Dbackend-wayland=enabled")))
    (native-inputs (list pkg-config
                         tllist
                         flex
                         bison
                         scdoc
                         wayland-protocols))
    (inputs (list fcft
                  wayland
                  pipewire
                  libyaml
                  pixman
                  alsa-lib
                  json-c
                  libmpdclient
                  pulseaudio
                  eudev))
    (synopsis "X11 and Wayland status panel")
    (description
     "@command{yambar} is a lightweight and configurable status panel (bar,
for short) for X11 and Wayland, that goes to great lengths to be both CPU and
battery efficient---polling is only done when absolutely necessary.")
    (license license:expat)))

(define-public wideriver
  (package
    (name "wideriver")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alex-courtis/wideriver")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16i0mzgxn32nrh5ajn0kb4xdwmsjg03amhasxhwyvspar5y4flhg"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)))) ; no configure script
    (native-inputs (list pkg-config cmocka))
    (inputs (list wayland wayland-protocols wlroots))
    (home-page "https://github.com/alex-courtis/wideriver")
    (synopsis "A set of riverWM layouts")
    (description
     "Tiling window manager for the river wayland compositor, inspired by dwm
and xmonad.")
    (license license:gpl3)))

(define-public wf-config
  (package
    (name "wf-config")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/WayfireWM/" name "/"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "07x6dapv2xyg0cn44dd2faw5gpk7mwfpbkpld9kyiaa9d44362z1"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list glm libxml2 wlroots-0.17 libevdev))
    (home-page "https://github.com/WayfireWM/wf-config")
    (synopsis "Library for managing configuration files for Wayfire")
    (description "The package provides a library for managing the
configuration files of Wayifre.  It can set key and mouse bindings,
configure input, and customize Wayfire plugins.")
    (license license:expat)))

(define-public scenefx
  (package
    (name "scenefx")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wlrfx/scenefx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jvbskpmhq0vs4rx9723n709h77zg4c0cid8jwnrag1hqh601ch4"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config
                         ;; for wayland-scanner.
                         wayland))
    (inputs (list pixman
                  mesa
                  libxkbcommon
                  libdrm
                  wlroots))
    (home-page "https://github.com/wlrfx/scenefx")
    (synopsis "Drop-in replacement for the wlroots scene API")
    (description
     "A drop-in replacement for the wlroots scene API that allows wayland
compositors to render surfaces with eye-candy effects.")
    (license license:expat)))
