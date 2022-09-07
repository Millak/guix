;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages vnc)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public remmina
  (package
    (name "remmina")
    (version "1.4.23")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/Remmina/Remmina")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j0fiz76z4y08w136vs8igqxxg42hx61r5hf6sylcr0c424sc9rk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:configure-flags
       (list
        ;; Disable online version checking.
        "-DWITH_NEWS=OFF")
       #:imported-modules
       ((guix build glib-or-gtk-build-system)
        ,@%cmake-build-system-modules)
       #:modules
       (((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build cmake-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'glib-or-gtk-wrap 'wrap-typelibs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("remmina" "remmina-file-wrapper"))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           intltool
           pkg-config))
    (inputs
     (list libappindicator
           atk
           avahi
           cairo
           cups
           ffmpeg
           freerdp                      ; for rdp plugin
           libgcrypt
           librsvg
           glib
           gnome-keyring
           gsettings-desktop-schemas
           gtk+
           harfbuzz
           json-glib
           libsecret                    ; for secret plugin
           libsoup-minimal-2
           libssh                       ; for ssh plugin
           libvnc                       ; for vnc plugin
           openssl
           pango
           pcre2                        ; for exec plugin
           shared-mime-info
           libsodium
           spice-gtk                    ; for spice plugin
           telepathy-glib
           vte                          ; for st plugin
           wayland
           webkitgtk                    ; for www plugin
           libx11
           libxext                      ; for xdmcp plugin
           xdg-utils
           libxkbfile))                 ; for nx plugin
    (propagated-inputs
     (list dconf))
    (home-page "https://remmina.org/")
    (synopsis "Remote Desktop Client")
    (description "Remmina is a client to use other desktops remotely.
RDP, VNC, SPICE, NX, XDMCP, SSH and EXEC network protocols are supported.")
    (license license:gpl2+)))

(define-public tigervnc-client
  ;; xorg-server 21 support was merged 2 weeks after the last (1.12.0) release.
  (let ((revision "0")
        (commit "b484c229853a08c7f254a4c6efbaf3c9e85b5074"))
    (package
      (name "tigervnc-client")
      (version (git-version "1.12.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TigerVNC/tigervnc")
               (commit commit)))
         (sha256
          (base32 "125dnn05ps7vfsxlxmzm05w99lhm8hk8j4hpxl1mlzb5j0hp1061"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f                 ; Tests that do exists are not automated.
                  #:phases (modify-phases %standard-phases
                             (replace 'install
                               (lambda* (#:key outputs #:allow-other-keys)
                                 (with-directory-excursion "vncviewer"
                                   (invoke "make" "install")))))))
      (native-inputs
       (list autoconf gettext-minimal automake))
      (inputs
       (list zlib
             gnutls
             libjpeg-turbo
             fltk
             linux-pam
             libx11
             libxext
             libxtst
             libxrandr
             libxdamage
             pixman))
      (home-page "https://tigervnc.org/")
      (synopsis "High-performance, platform-neutral
implementation of VNC (client)")
      (description "TigerVNC is a client/server implementation of VNC (Virtual
Network Computing).  It provides enough performance to run even 3D and video
applications.  It also provides extensions for advanced authentication methods
and TLS encryption.  This package installs only the VNC client, the
application which is needed to connect to VNC servers.")
      (license license:gpl2))))

;; A VNC server is, in fact, an X server so it seems like a good idea
;; to build on the work already done for xorg-server package.  This is
;; not entirely compatible with the recommendation in BUILDING.txt
;; where the client is built first, then the source code of the X
;; server is copied into a subdir of the build directory, patched with
;; VNC additions and then build and installed as Xvnc.  The procedure
;; was turned around, where TigerVNC code is downloaded and built
;; inside the Guix X server build dir. Also, the VNC patching process
;; for the X server is automated in a straightforward manner.
(define-public tigervnc-server
  (package
    (inherit xorg-server)
    (name "tigervnc-server")
    (version (package-version tigervnc-client))
    (native-inputs
     `(("tigervnc-src" ,(package-source tigervnc-client))
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext-minimal" ,gettext-minimal)
       ("font-util" ,font-util)
       ("cmake" ,cmake)
       ("perl" ,perl)
       ,@(package-native-inputs tigervnc-client)
       ,@(package-inputs tigervnc-client)
       ,@(package-native-inputs xorg-server)))
    (inputs
     (modify-inputs (package-inputs xorg-server)
       (prepend perl coreutils xauth)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs xorg-server)
       (prepend xauth)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments xorg-server)
       ((#:configure-flags flags)
        `(append '("--with-pic"         ; Taken from BUILDING.txt
                   "--without-dtrace"
                   "--disable-static"
                   "--disable-dri2"
                   "--disable-xinerama"
                   "--disable-xvfb"
                   "--disable-xnest"
                   "--disable-xorg"
                   "--disable-dmx"
                   "--disable-xwin"
                   "--disable-xephyr"
                   "--disable-kdrive"
                   ;; "--disable-config-dbus" ; This was a warning.
                   "--disable-config-hal"
                   "--disable-config-udev"
                   "--disable-dri2"
                   ;; "--enable-install-libxf86config" ; This, too, was a warning.
                   "--enable-glx")
                 (delete "--enable-xephyr" ,flags)))
       ((#:modules modules)
        `(append '((ice-9 ftw)
                   (ice-9 match)
                   (guix build utils)
                   (guix build gnu-build-system))
                 modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'check)              ;)
           (add-after 'unpack 'copy-tvnc-xserver
             (lambda _
               (let*
                   ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                    (tvnc-xserver (string-append tvnc-src "/unix/xserver")))
                 (copy-recursively tvnc-xserver "."))))
           (add-after 'copy-tvnc-xserver 'patch-xserver
             (lambda _
               (invoke "patch" "-p1" "-i"
                       (string-append (assoc-ref %build-inputs "tigervnc-src")
                                      "/unix/xserver21.1.1.patch"))
               (invoke "autoreconf" "-fiv")))
           (add-before 'build 'build-tigervnc
             (lambda _
               (let* ((out (assoc-ref %outputs "out"))
                      (tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                      (tvnc-build (string-append (getcwd) "/tigervnc-build")))
                 (mkdir-p tvnc-build)
                 (with-directory-excursion tvnc-build
                   (invoke "cmake" "-G" "Unix Makefiles"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           tvnc-src)
                   (invoke "make" "-j" (number->string (parallel-job-count)))))))
           (replace 'build
             (lambda _
               (let*  ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (invoke "make" srcarg buildarg "-j"
                         (number->string (parallel-job-count))))))
           (add-before 'install 'install-tigervnc-aux
             (lambda _
               (let*  ((out (assoc-ref %outputs 'out))
                       (tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (with-directory-excursion (string-append tvnc-build "/unix")
                   (invoke "make" srcarg buildarg "install")))))
           (replace 'install
             (lambda* _
               (let*  ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (invoke "make" "install" srcarg buildarg))))))))
    (description "TigerVNC is a client/server implementation of VNC (Virtual
Network Computing).  It provides enough performance to run even 3D and video
applications.  It also provides extensions for advanced authentication methods
and TLS encryption.  This package installs the VNC server, a program that will
enable users with VNC clients to log into a graphical session on the machine
where the server is installed.")))

(define-public turbovnc
  (package
    (name "turbovnc")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/turbovnc/" version
                           "/turbovnc-" version ".tar.gz"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; There are a few bundled Java libraries, such as jsch and jzlib,
            ;; bundled under java/com/jcraft/ as well as mindrot and spf4j,
            ;; bundled under java/org.  These are used by the 'vncviewer'
            ;; program.  The jsch copy is modified and integrates changes from
            ;; https://github.com/mwiede/jsch, so cannot easily be un-bundled.
            (define (directory? x)
              (and=> (stat x #f) (compose (cut eq? 'directory <>) stat:type)))

            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <> (append '("." "..")
                                                            preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))

            ;; d3des, rfb (headers) and turbojpeg-jni are small and not
            ;; packaged in Guix, so preserve them.
            (delete-all-but "common" "d3des" "rfb" "turbojpeg-jni")
            ;; Delete bundled headers which aren't used.
            (delete-all-but "unix/Xvnc/include" "tvnc_version.h.in")
            ;; This 243 lines of code C library is used by
            ;; unix/Xvnc/programs/Xserver/os/xsha1.c.
            (delete-all-but "unix/Xvnc/lib" "CMakeLists.txt" "libsha1")
            (delete-file-recursively "unix/Xvnc/extras")))
       (sha256
        (base32
         "182amp471qvr2cn2rbw97zpbkh9q7mf92w1r25cg4apx5k26m7c3"))
       (patches (search-patches "turbovnc-find-system-packages.patch"
                                "turbovnc-custom-paths.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:configure-flags
      ;; Use system libraries.
      #~(list "-DTVNC_SYSTEMLIBS=ON"
              "-DTVNC_SYSTEMX11=ON"
              "-DTVNC_DLOPENSSL=OFF"
              (string-append "-DXORG_DRI_DRIVER_PATH="
                             (search-input-directory %build-inputs "lib/dri"))
              (string-append "-DXORG_FONT_PATH="
                             "/run/current-system/profile/share/fonts/X11,"
                             (string-append #$(this-package-input "font-alias")
                                            "share/fonts/X11"))
              (string-append "-DXORG_REGISTRY_PATH="
                             (dirname (search-input-file
                                       %build-inputs "lib/xorg/protocol.txt")))
              (string-append "-DXKB_BASE_DIRECTORY="
                             (search-input-directory %build-inputs
                                                     "share/X11/xkb"))
              (string-append "-DXKB_BIN_DIRECTORY="
                             (dirname (search-input-file %build-inputs
                                                         "bin/xkbcomp")))
              ;; The default rule is 'xorg', which doesn't match the 'base'
              ;; rule file installed by our version of xkeyboard-config.
              ;; Without this change, running Xvnc would fail with the error
              ;; "XKB: Failed to compile keymap"
              "-DXKB_DFLT_RULES=base"
              ;; Mimic xorg-server's "--with-xkb-output=/tmp" configuration.
              "-DCOMPILEDDEFAULTFONTPATH=/tmp"
              "-DTVNC_STATIC_XORG_PATHS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-vncviewer
            (lambda* (#:key inputs #:allow-other-keys)
              (define openjdk #$(this-package-input "openjdk"))
              (substitute* "unix/vncviewer/vncviewer.in"
                (("\\$BINDIR/../java/jre")
                 openjdk)
                ;; Avoid resorting to grep and sed to locate libjawt.so.
                (("^_TMP=.*")
                 (string-append "_TMP=" openjdk "/lib\n")))))
          (add-after 'unpack 'patch-xstartup.turbovnc
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "unix/xstartup.turbovnc"
                (("DBUS_LAUNCH=[[:graph:]]+")
                 (format #f "DBUS_LAUNCH=~a"
                         (search-input-file inputs "bin/dbus-launch")))
                (("XSESSIONSDIR=[[:graph:]]+")
                 (format #f "XSESSIONSDIR=~a"
                         "/run/current-system/profile/share/xsessions"))
                (("GREP=[[:graph:]]+")
                 (format #f "GREP=~a"
                         (search-input-file inputs "bin/grep")))
                (("SED=[[:graph:]]+")
                 (format #f "SED=~a"
                         (search-input-file inputs "bin/sed")))
                (("TVNC_SSHAGENT=[[:graph:]]+")
                 (format #f "TVNC_SSHAGENT=~a"
                         (search-input-file inputs "bin/ssh-agent")))
                (("TVNC_VGLRUN=\"vglrun" all)
                 (string-append "TVNC_VGLRUN="
                                (search-input-file inputs "bin/vglrun") all)))))
          (add-after 'install 'wrap-vncserver
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-script (search-input-file outputs "bin/vncserver")
                (list "PATH" 'prefix
                      (map (lambda (p)
                             (dirname (search-input-file inputs p)))
                           '("bin/uname" ;coreutils
                             "bin/xauth"
                             "bin/xdpyinfo"))))))
          (add-after 'install 'wrap-xstartup.turbovnc
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-script (search-input-file outputs "bin/xstartup.turbovnc")
                (list "PATH" 'prefix
                      (map (lambda (p)
                             (dirname (search-input-file inputs p)))
                           '("bin/uname" ;coreutils
                             ;; These are used as the fallback when no desktop
                             ;; session was found.
                             "bin/twm"
                             "bin/xsetroot"
                             "bin/xterm")))))))))
    (native-inputs
     (list `(,openjdk "jdk")
           pkg-config
           python))
    (inputs
     (list dbus
           font-alias
           freetype
           guile-3.0
           libfontenc
           libjpeg-turbo
           libx11
           libxdamage
           libxext
           libxfont2
           libxi
           libxkbfile
           linux-pam
           mesa
           openjdk
           openssh
           openssl
           perl
           pixman
           twm
           virtualgl
           xauth
           xdpyinfo
           xkbcomp
           xkeyboard-config
           xorg-server
           xorgproto
           xsetroot
           xterm
           xtrans
           zlib))
    (home-page "https://turbovnc.org/")
    (synopsis "Highly-optimized VNC remote desktop software")
    (description "TurboVNC is a high-speed version of VNC derived from
TightVNC, with which it remains compatible.  It contains a variant of Tight
encoding that is tuned to maximize performance for image-intensive
applications (such as VirtualGL, video applications, and image editors) while
still providing excellent performance for other types of applications.  Some
of its unique features are:
@itemize
@item a user-facing @command{vncserver} command;
@item the ability to capture keyboard keys even when not in full screen mode;
@item a full screen mode that is compatible with ratpoison*
@end itemize
*Although due to a quirk in Java, you'll want to set the
@env{_JAVA_AWT_WM_NONREPARENTING} environment variable when using it with
ratpoison.")
    (license license:gpl2+)))

(define-public libvnc
  (package
    (name "libvnc")
    (version "0.9.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LibVNC/libvncserver")
             (commit (string-append "LibVNCServer-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zz0hslw8b1p3crnfy3xnmrljik359h83dpk64s697dqdcrzy141"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-cc-reference
                    (lambda _
                      (substitute* "test/includetest.sh"
                        (("^cc -I")
                         "gcc -I"))
                      #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("lzo" ,lzo)
       ("sdl2" ,sdl2)))
    (home-page "https://libvnc.github.io/")
    (synopsis "Cross-platform C libraries for implementing VNC server or
client")
    (description "This package provides @code{LibVNCServer} and
@code{LibVNCClient}.  These are cross-platform C libraries that allow you to
easily implement VNC server or client functionality in your program.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))
