;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021, 2022, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Mehmet Tekman <mtekman89@gmail.com>
;;; Copyright @ 2022, Kitzman <kitzman@disroot.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
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
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
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
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public remmina
  (package
    (name "remmina")
    (version "1.4.35")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/Remmina/Remmina.git")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wsjhmazmv888a6xs0q6llfj6v81d7y9p18w7xc2116235q9ygfk"))))
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
           at-spi2-core
           avahi
           bash-minimal
           cairo
           cups
           curl
           ffmpeg
           freerdp                      ; for rdp plugin
           libgcrypt
           (librsvg-for-system)
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
           python-minimal-wrapper       ; for python wrapper plugin
           shared-mime-info
           libsodium
           spice-gtk                    ; for spice plugin
           telepathy-glib
           vte/gtk+-3                   ; for st plugin
           wayland
           webkitgtk-for-gtk3           ; for www plugin
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
  (package
    (name "tigervnc-client")
    (version "1.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TigerVNC/tigervnc")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1n6slj7i93gvf0ji4mb3azycv3c4wqzfd7zlx9260b79jv8jvsln"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                   ; tests that do exists are not automated
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
           ;;ffmpeg                     ;TODO: add this for h264 encoding
           fltk-1.3
           linux-pam
           libx11
           libxext
           libxtst
           libxrandr
           libxdamage
           pixman))
    (home-page "https://tigervnc.org/")
    (synopsis "High-performance VNC remote desktop client")
    (description "TigerVNC implements a @acronym{VNC, Virtual Network Computing}
client and server.  @dfn{VNC} is a remote display system that lets you view and
interact with a virtual desktop environment running on another computer on the
network.  Client and server may be running on different operating systems and
architectures.

TigerVNC uses a variant of Tight encoding that is greatly accelerated by the use
of the libjpeg-turbo JPEG codec and performs fast enough to run even 3D or video
applications.  It also provides extensions for advanced authentication methods
and @acronym{TLS, Transport-Level Security} encryption.

This package installs only the VNC client (@command{vncviewer}), the application
used to connect to VNC servers such as the tigervnc-server package.")
    (license license:gpl2)))

(define %tigervnc-client-source (package-source tigervnc-client))

;; A VNC server is, in fact, an X server so it seems like a good idea to build
;; on the work already done for xorg-server package.  This is not entirely
;; compatible with the recommendation in BUILDING.txt where the client is
;; built first, then the source code of the X server is copied into a subdir
;; of the build directory, patched with VNC additions and then build and
;; installed as Xvnc.  The procedure was turned around, where TigerVNC code is
;; downloaded and built inside the Guix X server build dir.  Also, the VNC
;; patching process for the X server is automated in a straightforward manner.
(define-public tigervnc-server
  (package
    (inherit xorg-server)
    (name "tigervnc-server")
    (version (package-version tigervnc-client))
    (source
     (origin
       (inherit (package-source xorg-server))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Copy the VNC extension into the xorg-server sources.
            (copy-recursively #$(file-append %tigervnc-client-source
                                             "/unix/xserver")
                              ".")
            ;; Include a full copy of tigervnc-client sources, so that the
            ;; complete sources involved are available and can be edited during
            ;; the build.
            (copy-recursively #$%tigervnc-client-source "tigervnc-client")
            ;; Adjust the VNC extension build system files so that it refers
            ;; to it.
            (substitute* "hw/vnc/Makefile.am"
              (("(TIGERVNC_SRCDIR=).*" _ head)
               (string-append head "$(CURDIR)/../../tigervnc-client\n"))
              (("(TIGERVNC_BUILDDIR=).*" _ head)
               (string-append head
                              "$(CURDIR)/../../tigervnc-client/build\n")))
            ;; Ensure the Autotools build system gets re-bootstrapped.
            (delete-file "configure")))
       ;; Patch the xorg-server build system so that it builds the VNC
       ;; extension.
       (patches (cons (file-append %tigervnc-client-source
                                   "/unix/xserver21.patch")
                      (origin-patches (package-source xorg-server))))
       (file-name (string-append name "-" version ".tar.xz"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments xorg-server)
       ((#:tests? #f #f)
        #f)
       ((#:configure-flags flags)
        #~(cons* "--with-pic"           ; taken from BUILDING.txt
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
                 "--disable-config-hal"
                 "--disable-config-udev"
                 "--disable-dri2"
                 "--enable-glx"
                 (delete "--enable-xephyr" #$flags)))
       ((#:modules modules)
        `(append '((ice-9 ftw)
                   (ice-9 match)
                   (guix build utils)
                   (guix build gnu-build-system))
                 modules))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'adjust-pam-config
              (lambda _
                (substitute* "tigervnc-client/unix/vncserver/tigervnc.pam"
                  (("pam_systemd.so")
                   "pam_elogind.so"))))
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "tigervnc-client/unix/vncserver/vncserver.in"
                  (("`mcookie`")
                   (format #f "`~a`" (search-input-file inputs "bin/mcookie")))
                  ;; Adjust the places where the vncserver script looks for
                  ;; X11 fonts.
                  (("'/usr/share/X11/fonts'" all)
                   (format #f "'~a', '~a', ~a"
                           "/run/current-system/profile/share/fonts/X11"
                           (string-append #$(this-package-input "font-alias")
                                          "share/fonts/X11")
                           all))
                  ;; Adjust the location where .desktop files will be saved.
                  (("/usr/share/xsessions")
                   "/run/current-system/profile/share/xsessions")
                  ;; Do not require a system-provided Xsession shell script.
                  ;; Guix System has none, causing the for loop to iterate
                  ;; over an empty list.
                  (("\"/etc/X11/xinit/Xsession\", \"/etc/X11/Xsession\"")
                   "()")
                  (("if \\(not defined \\$Xsession)")
                   "if (0)")
                  (("@cmd, \\$Xsession,")
                   "@cmd,"))))
            (add-before 'build 'build-tigervnc
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (mkdir-p "tigervnc-client/build")
                (with-directory-excursion "tigervnc-client/build"
                  (invoke "cmake" "-G" "Unix Makefiles"
                          (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                          "..")
                  (invoke "make" "-j" (number->string (if parallel-build?
                                                          (parallel-job-count)
                                                          1))))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (invoke "make" "-j" (number->string (if parallel-build?
                                                        (parallel-job-count)
                                                        1)))))
            (add-before 'install 'install-tigervnc-aux
              (lambda _
                (invoke "make" "-C" "tigervnc-client/build/unix" "install")))
            (replace 'install
              (lambda _
                (invoke "make" "install")))
            (add-after 'install 'wrap-vncserver
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (wrap-script (search-input-file outputs "libexec/vncserver")
                  (list "PATH" 'prefix
                        (map (lambda (p)
                               (dirname (search-input-file inputs p)))
                             '("bin/uname"
                               "bin/xauth"
                               "bin/xinit"))))))))))
    (native-inputs
     (modify-inputs (append (package-native-inputs xorg-server)
                            (package-native-inputs tigervnc-client))
       (append %tigervnc-client-source
               autoconf
               automake
               libtool
               gettext-minimal
               font-util
               cmake
               perl)))
    (inputs
     (modify-inputs (append (package-inputs xorg-server)
                            (package-inputs tigervnc-client))
       (prepend coreutils
                font-alias
                guile-3.0
                perl
                util-linux
                xauth
                xinit)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs xorg-server)
       (prepend xauth)))
    (synopsis "High-performance VNC remote desktop server based on Xorg")
    (description "TigerVNC implements a @acronym{VNC, Virtual Network Computing}
client and server.  @dfn{VNC} is a remote display system that lets you view and
interact with a virtual desktop environment running on another computer on the
network.  Client and server may be running on different operating systems and
architectures.

TigerVNC uses a variant of Tight encoding that is greatly accelerated by the use
of the libjpeg-turbo JPEG codec and performs fast enough to run even 3D or video
applications.  It also provides extensions for advanced authentication methods
and @acronym{TLS, Transport-Level Security} encryption.

This package installs the VNC server.  Permitted users can log into a graphical
session on the machine where the server is running, using a VNC client such as
the tigervnc-client package.")))

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

(define-public x11vnc
  ;; The release version of 0.9.16 requires patches to work, so we pin to the
  ;; latest working commit
  (let ((commit "3e4dc8ef2985a6e670e1d9649fe55395c2b31039")
        (revision "0"))
    (package
      (name "x11vnc")
      (version (git-version "0.9.16" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/LibVNC/x11vnc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a120gv9h3whiznlddl0j3nz3400jjgl97znaincm5i2m5pnjifs"))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases #~(modify-phases %standard-phases
                          (add-before 'bootstrap 'delete-premature-configure
                            (lambda _
                              (substitute* "./autogen.sh"
                                ((".*/configure")
                                 "")))))))
      (native-inputs (list autoconf automake autobuild pkg-config))
      (inputs (list avahi
                    libjpeg-turbo
                    libvnc
                    libx11
                    libxcomposite
                    libxdamage
                    libxext
                    libxfixes
                    libxi
                    libxinerama
                    libxrandr
                    libxtst
                    openssl
                    xdpyinfo
                    xf86-video-dummy
                    zlib))
      (synopsis "VNC server for real X displays")
      (home-page "https://github.com/LibVNC/x11vnc")
      (description
       "x11vnc allows one to view and interact with real remote X
displays (i.e. a display corresponding to a physical monitor, keyboard, and
mouse) with any VNC viewer.")
      (license license:gpl2+))))

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

(define-public neatvnc
  (package
    (name "neatvnc")
    (version "0.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/any1/neatvnc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09vafk99zmrbrb5mxr1sqb21rvggbr69kx7rwqf2g6dxk07p1mqg"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list libdrm libglvnd libxkbcommon pixman aml gnutls libjpeg-turbo zlib))
    (home-page "https://github.com/any1/neatvnc")
    (synopsis "Lightweight VNC server library")
    (description "NeatVNC is a lightweight VNC server library, supporting
authentication, SSH tunneling, and ZRLE or Tight encoding.")
    (license license:isc)))

(define-public wayvnc
  (package
    (name "wayvnc")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/any1/wayvnc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1brnzwabnrhjblcfymwxsg4z58pzdnlql1mgsmijp0kw5n8770rc"))))
    (build-system meson-build-system)
    (native-inputs
     (append (if (%current-target-system)
                 ;; for wayland-scanner
                 (list wayland)
                 '())
             (list pkg-config scdoc)))
    (inputs (list aml
                  neatvnc
                  zlib
                  libjpeg-turbo
                  gnutls
                  jansson
                  libdrm
                  pixman
                  libglvnd
                  libxkbcommon
                  wayland))
    (home-page "https://github.com/any1/wayvnc")
    (synopsis "VNC server for wlroots-based Wayland compositors")
    (description
     "This is a VNC server for wlroots-based Wayland compositors.
It attaches to a running Wayland session, creates virtual input devices, and
exposes a single display via the RFB protocol.  The Wayland session may be a
headless one, so it is also possible to run wayvnc without a physical display
attached.")
    (license license:isc)))
