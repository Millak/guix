;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2025 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2026 Dariqq <dariqq@posteo.net>
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

(define-module (gnu packages gnustep)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public fsviewer
  (package
    (name "fsviewer")
    (version "0.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/roblillack/fsviewer")
              (commit (string-append "v" version))))
       (sha256
        (base32
         "067fvbq7b4lbnn1flmwf8lm0vq4n6qz5yz6hhab30l4gksfk51cg"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;no tests
    (inputs
     (list windowmaker libxmu libxpm))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/roblillack/fsviewer")
    (synopsis "File viewer for Window Maker")
    (description "FSViewer is a NeXT FileViewer lookalike for Window
Maker supporting browser mode and list mode.  This version is a fork of
FSViewer.app originally written by George Clernon (1998-99) and later
maintained by Guido Scholz (2002-07).")
    (license license:gpl2)))

(define gnustep-dirmap
  ''(("GNUSTEP_SYSTEM_APPS"        . "lib/GNUstep/Applications")
     ("GNUSTEP_SYSTEM_ADMIN_APPS"  . "lib/GNUstep/Applications")
     ("GNUSTEP_SYSTEM_WEB_APPS"    . "lib/GNUstep/WebApplications")
     ("GNUSTEP_SYSTEM_TOOLS"       . "bin")
     ("GNUSTEP_SYSTEM_ADMIN_TOOLS" . "sbin")
     ("GNUSTEP_SYSTEM_LIBRARY"     . "lib/GNUstep")
     ("GNUSTEP_SYSTEM_HEADERS"     . "include")
     ("GNUSTEP_SYSTEM_LIBRARIES"   . "lib")
     ("GNUSTEP_SYSTEM_DOC"         . "share/GNUstep/Documentation")
     ("GNUSTEP_SYSTEM_DOC_MAN"     . "share/man")
     ("GNUSTEP_SYSTEM_DOC_INFO"    . "share/info")))

(define-public (make-gnustep-make-flags out)
  `(map
    (lambda (dir)
      (string-append (car dir) "=" ,out "/" (cdr dir)))
    ,gnustep-dirmap))

(define-public gnustep-make
  (package
    (name "gnustep-make")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")
                    (string-append "ftp://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "092cy3clchx381qrc8s7gxabqarz9jfsh3bnag5fnyb2f05k5jlk"))
              (patches
               (search-patches
                "gnustep-make-makefiles-search-path.patch"
                "gnustep-make-no-temp.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~'("-with-layout=fhs-system")
      #:make-flags #~'("GNUSTEP_INSTALLATION_DOMAIN=SYSTEM"
                       ;; Don't build pdf documentation.
                       "LATEX_FILES=")
      #:tests? #f ;; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-common-makefile
            (lambda _
              (patch-makefile-SHELL "common.make")))
          (add-after 'install 'install-doc
            (lambda args
              (with-directory-excursion "Documentation"
                (apply (assoc-ref %standard-phases 'install) args)))))))
    (native-inputs
     (list which
           texinfo))
    (native-search-paths
     (list
      (search-path-specification
        (variable "GUIX_GNUSTEP_MAKEFILES_ADDITIONAL")
        (file-type 'regular)
        (file-pattern "^.*\\.make$")
        (files
         (list "share/GNUstep/Makefiles/Additional")))))
    (home-page "https://gnustep.org")
    (synopsis "GNUstep make package")
    (description "The makefile package is a simple, powerful and extensible way
to write makefiles for a GNUstep-based project.  It allows the user to write a
project without having to deal with the complex issues associated with
configuration, building, installation, and packaging.  It also allows the user
to easily create cross-compiled binaries.")
    (license license:gpl3+)))

(define-public gnustep-base
  (package
    (name "gnustep-base")
    (version "1.31.1")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")
                    (string-append "ftp://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "1ka50m36ld2ggd4h8j2snhf947m6jh0kcflmfsv7az4ajwf6ym77"))
              (patches
               (search-patches
                "gnustep-base-autogsdoc-reproducibility.patch"
                "gnustep-base-tzdir.patch"
                "gnustep-base-search-paths.patch"
                "gnustep-base-no-builtin-tz.patch"
                "gnustep-base-disable-network-tests.patch"
                "gnustep-base-chroot-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~`("--with-installation-domain=SYSTEM"
                            ,(string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:make-flags #~`("messages=yes"
                       "GNUSTEP_TZ=UTC"
                       "GNUSTEP_INSTALLATION_DOMAIN=SYSTEM"
                       ;; override config from gnustep-make
                       ,@#$(make-gnustep-make-flags '(assoc-ref %outputs "out")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-makefile
            ;; install fails because it wants to install into gnustep-make
            (lambda _
              (substitute* "Makefile.postamble"
                (("\\$\\(GNUSTEP_MAKEFILES\\)")
                 (string-append #$output "/share/GNUstep/Makefiles")))))
          (add-before 'check 'pre-check
            (lambda _
              (substitute* "Tests/base/NSURL/basic.m"
                ;; Use a path that exists
                (("/usr") "/tmp"))))
          (add-after 'install 'install-doc
            (lambda args
              (with-directory-excursion "Documentation"
                (apply (assoc-ref %standard-phases 'install) args)))))))
    (native-inputs
     (list gnustep-make which pkg-config
           texinfo
           tzdata-for-tests))
    (inputs
     (list curl
           gmp
           gnutls
           icu4c
           libxml2
           libxslt
           libffi
           `(,zstd "lib")
           zlib))
    (native-search-paths
     (map
      (lambda (dir)
        (search-path-specification
          (variable (string-append "GUIX_" (car dir)))
          (files (list (cdr dir)))))
      (cadr gnustep-dirmap)))
    (home-page "https://gnustep.org")
    (synopsis "Library of general-purpose, Objective C objects for GNUstep")
    (description "The GNUstep Base Library is a library of general-purpose,
 non-graphical Objective C objects. For example, it includes classes for
strings, object collections, byte streams, typed coders, invocations,
notifications, notification dispatchers, moments in time, network ports,
remote object messaging support (distributed objects), and event loops.

It provides functionality that aims to implement the non-graphical
portion of the Apple's Cocoa frameworks (the Foundation library)
which came from the OpenStep standard.")
    (license (list license:gpl3+ license:lgpl2.1+))))

(define-public gnustep-gui
  (package
    (name "gnustep-gui")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")
                    (string-append "ftp://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "1xm1z6kwz9y3dqqf8k4s88kynxqz14jviz2qxj9dbarv66va20qc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~`("--with-installation-domain=SYSTEM"
                            ,(string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib"))
      #:make-flags #~`("messages=yes" "doc=yes"
                       "GNUSTEP_TZ=UTC"
                       "GNUSTEP_INSTALLATION_DOMAIN=SYSTEM"
                       ,@#$(make-gnustep-make-flags '(assoc-ref %outputs "out")))
      #:validate-runpath? #f ;; bundles are dlopened and lack rpath
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-makefile
            ;; install fails because it wants to install into gnustep-make
            (lambda _
              (substitute* "GNUmakefile.postamble"
                (("\\$\\(GNUSTEP_MAKEFILES\\)")
                 (string-append #$output "/share/GNUstep/Makefiles"))))))))
    (native-inputs
     (list gnustep-make pkg-config
           texinfo which))
    (inputs
     (list gnustep-base
           aspell
           ;; cups
           giflib  ;; or libungif
           icu4c
           libjpeg-turbo
           libpng
           libtiff))
    (home-page "https://gnustep.org")
    (synopsis "Graphical user interface class library for GNUstep")
    (description "The GNUstep gui library is a library of graphical
 user interface classes written completely in the Objective-C language;
the classes are based upon Apple's Cocoa framwork (which came from the
OpenStep specification). These classes include graphical objects such
as buttons, text fields, popup lists, browser lists, and windows; there
 are also many associated classes for handling events, colors, fonts,
pasteboards and images.")
    (license (list license:gpl3+ license:lgpl2.1+))))

(define-public gnustep-back
  (package
    (name "gnustep-back")
    (version "0.32.0")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")
                    (string-append "ftp://ftp.gnustep.org/pub/gnustep/core/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "1391g1j710sib8fym0arm80gs4g0f5f5la71yr2xii9a06ai05yf"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~'("--with-installation-domain=SYSTEM")
      #:make-flags #~`("messages=yes"
                       "GNUSTEP_TZ=UTC"
                       "GNUSTEP_INSTALLATION_DOMAIN=SYSTEM"
                       ,@#$(make-gnustep-make-flags '(assoc-ref %outputs "out")))
      #:modules `((ice-9 popen)
                  (ice-9 rdelim)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-GNUSTEP_MAKEFILES
            ;; Ref https://github.com/gnustep/libs-back/issues/54
            (lambda _
              (let* ((port  (open-input-pipe "gnustep-config --variable=GNUSTEP_MAKEFILES"))
                     (makefiles (read-line port)))
                (close-pipe port)
                (setenv "GNUSTEP_MAKEFILES" makefiles))))
          (add-after 'install 'install-doc
            (lambda args
              (with-directory-excursion "Documentation"
                (apply (assoc-ref %standard-phases 'install) args)))))))
    (native-inputs
     (list gnustep-make
           pkg-config
           which
           texinfo))
    (inputs
     (list cairo
           freetype
           gnustep-base
           gnustep-gui

           libxt
           libxrandr
           libxmu
           libxft))
    (home-page "https://gnustep.org")
    (synopsis "Back-end library for drawing GNUstep GUIs")
    (description "Back-end component for the GNUstep GUI Library. The
implementation of the GNUstep GUI Library is designed in two parts. The
first part is the front-end component which is independent of platform
and display system. This front-end is combined with a back-end
component which handles all of the display system dependent such as
specific calls to the X Window System. This design allows the GNUstep
applications to have the \"look and feel\" of the underlying display
system without any changes to the application, and the library can be
easily ported to other display systems.")
    (license (list license:gpl3+ license:lgpl2.1+))))

(define-public libobjc2
  (package
    (name "libobjc2")
    (version "2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gnustep/libobjc2")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0790mxwq1na5gm5czi3phw384r44lfckb7wcp1hx1hjyksmg1c0b"))
              (file-name (git-file-name name version))
              (patches
               (search-patches "libobjc2-clang22.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; NOTE: Though the issue of GCC cannot compile ObjC code described
      ;; in https://issues.guix.gnu.org/29644 is fixed.
      ;; But libobjc2 says it 'strongly recommended that you compile with clang'
      #:configure-flags #~(list "-DCMAKE_C_COMPILER=clang"
                                "-DCMAKE_CXX_COMPILER=clang++"
                                "-DTESTS=ON")))
    (native-inputs
     (list clang))
    (inputs
     (list robin-map))
    (home-page "https://www.gnustep.org/")
    (synopsis "Objective-C runtime library for Clang")
    (description "Libobjc2 is an Objective-C runtime library designed as a
drop-in replacement for GCC runtime.  It supports following features beyond
GCC runtime.

@itemize
@item Modern Objective-C runtime APIs.
@item Blocks (Closures).
@item Synthesised property accessors.
@item Efficient support for @code{@@synchronized()}.
@item Type-dependent dispatch.
@item Associated reference API.
@item Automatic Reference Counting.
@end itemize")
    (license license:expat)))

(define-public windowmaker
  (package
    (name "windowmaker")
    (version "0.96.0")
    (synopsis "NeXTSTEP-like window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/window-maker/wmaker/releases/download/"
                    "wmaker-" version "/WindowMaker-" version ".tar.gz"))
              (sha256
               (base32
                "0vbgjbqnv2gcpimnrh2fc383gxcdfhgz1j3f2lfa4jng4fx31qag"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'pre-configure
            (lambda _
              ;; 'wmaker' wants to invoke 'wmaker.inst' the first time,
              ;; and the 'wmsetbg', so make sure it uses the right ones.
              ;; We can't use a wrapper here because that would pollute
              ;; $PATH in the whole session.
              (let* ((bin (string-append #$output "/bin")))
                (substitute* "src/main.c"
                  (("\"wmaker\\.inst")
                   (string-append "\"" bin "/wmaker.inst")))
                (substitute* '("src/defaults.c" "WPrefs.app/Menu.c")
                  (("\"wmsetbg")
                   (string-append "\"" bin "/wmsetbg")))
                ;; Add enough cells to the command character array to
                ;; allow passing our large path to the wmsetbg binary.
                ;; The path to wmsetbg in Guix requires 67 extra characters.
                (substitute* "src/defaults.c"
                  (("len = strlen\\(text\\) \\+ 40;")
                   (string-append "len = strlen(text) + 107;"))))))
          (add-after 'install 'install-xsession
            (lambda _
              (let* ((xsessions (string-append #$output "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/windowmaker.desktop")
                  (lambda (port)
                    (format port "~
                    [Desktop Entry]~@
                    Name=Window Maker~@
                    Comment=~a~@
                    Exec=~a/bin/wmaker~@
                    Type=Application~%"
                            (string-map (match-lambda
                                          (#\newline #\space)
                                          (chr chr))
                                        #$synopsis) #$output))))))
          (add-after 'install-xsession 'wrap
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                ;; In turn, 'wmaker.inst' wants to invoke 'wmmenugen'
                ;; etc., so make sure everything is in $PATH.
                (wrap-program (string-append bin "/wmaker.inst")
                  `("PATH" ":" prefix (,bin)))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           fontconfig
           giflib
           libjpeg-turbo
           libpng
           libtiff
           libx11
           libxft
           libxinerama
           libxmu))
    (native-inputs
     (list pkg-config))
    (home-page "https://windowmaker.org/")
    (description
     "Window Maker is an X11 window manager originally designed to provide
integration support for the GNUstep Desktop Environment.  In every way
possible, it reproduces the elegant look and feel of the NeXTSTEP user
interface.  It is fast, feature rich, easy to configure, and easy to use.")

    ;; Artwork is distributed under the WTFPL.
    (license license:gpl2+)))

(define-public wmbattery
  (package
    (name "wmbattery")
    (version "2.54")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmbattery/wmbattery_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "1r4n58mwkm69y1pjs7l64hg8r1lpndrzyrfl2rdgd4zi6v0jhyyw"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; no "check" target
    (inputs
     (list glib libx11 libxext libxpm upower))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.dockapps.net/wmbattery")
    (synopsis "Display laptop battery info")
    (description
     "Wmbattery displays the status of your laptop's battery in a small icon.
This includes if it is plugged in, if the battery is charging, how many minutes
of battery life remain, battery life remaining (with both a percentage and a
graph), and battery status (high - green, low - yellow, or critical - red).")
    (license license:gpl2)))

(define-public wmnd
  (package
    (name "wmnd")
    (version "0.4.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thregr.org/~wavexx/software/wmnd/releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "01s37d8cfpncza1mlw13ar4rcwbrc1vgaj3ifhglmlcnzvvayg0n"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.thregr.org/~wavexx/software/wmnd/")
    (synopsis "Network interface monitor")
    (description
     "WMND is a dockapp for monitoring network interfaces under WindowMaker and
other compatible window managers.")
    (license license:gpl2+)))

(define-public wmcpuload
  (package
    (name "wmcpuload")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmcpuload/"
                    name "_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1334y0axnxydwv05d172f405iljrfakg4kcyg9kmn46v6ywv424g"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.dockapps.net/wmcpuload")
    (synopsis "Monitor CPU usage")
    (description
     "Wmcpuload displays the current CPU usage, expressed as a percentile and a
chart, and has an LCD look-alike user interface.  The back-light may be turned
on and off by clicking the mouse button over the application.  If the CPU usage
hits a certain threshold, an alarm-mode will alert you by turning back-light
on.")
    (license license:gpl2+)))

(define-public wmclock
  (package
    (name "wmclock")
    (version "1.0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmclock/"
                    name "_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1lx276ba8r2yydhmwj1g586jdqg695ad89ng36fr3mb067gvb2rz"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    ;; wmclock requires autoreconf to generate its configure script.
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://www.dockapps.net/wmclock")
    (synopsis "Display the date and time")
    (description
     "wmclock is an applet for Window Maker which displays the date and time in
a dockable tile.  It features multiple language support, 24h or 12h time
display, and can run a user-specified program on mouse click.")
    (license license:gpl2+)))

(define-public wmfire
  (package
    (name "wmfire")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.improbability.net/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "101grahd80n97y2dczb629clmcgiavdpbbwy78kk5wgs362m12z3"))
              (patches
               (search-patches "wmfire-dont-inline-draw-fire.patch"
                               "wmfire-update-for-new-gdk-versions.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 libgtop))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.improbability.net/")
    (synopsis "Display flames to represent resource usage")
    (description
     "wmfire is an applet for Window Maker that can monitor the average cpu
load, or individual cpu load on SMP computers.  Additionally it can monitor the
memory, network load, a file or just be set to show a pretty flame.  On
entering the dock a burning spot replaces the cursor, and after two seconds
symbols to represent the current monitor are \"burnt\" onscreen.  The flame
colour can also be changed.")
    (license license:gpl2+)))

(define-public wmamixer
  (package
    (name "wmamixer")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gryf/wmamixer")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04vv4kr4mj1nwri6zqgdg4yzbbmmng73qd4h0azliril75m7sldf"))))
    (inputs (list libx11 libxpm libxext alsa-lib))
    (build-system gnu-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (delete 'check)
                        (replace 'install
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (let* ((out (assoc-ref outputs "out"))
                                   (bin (string-append out "/bin")))
                              (install-file "wmamixer" bin)))))))
    (synopsis "Window maker applet to display the current volume")
    (description
     "wmamixer is an applet for window maker which displays the
current volume level both numerically and visiaully with a volume bar.  It
includes the ability to toggle through different outputs to show their
respective volume level.")
    (home-page "https://github.com/gryf/wmamixer")
    (license license:gpl2+)))

