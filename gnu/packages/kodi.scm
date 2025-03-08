;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages kodi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages assembly))

(define-public crossguid
  (let ((commit "ca1bf4b810e2d188d04cb6286f957008ee1b7681")
        (revision "3"))
    (package
      (name "crossguid")
      (version (string-append "0.0-" revision "." (string-take commit 7)))
      ;; This is the commit that Kodi wants.
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/graeme-hill/crossguid")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1x3jc4q6di79x3nlx36394s03yv1j1j5k0x6zljyk5iq78y4mfyz"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f))
      (inputs
       `(("libuuid" ,util-linux "lib")))
      (synopsis "Lightweight universal identifier library")
      (description "CrossGuid is a minimal GUID/UUID
generator library for C++.")
      (home-page "https://github.com/graeme-hill/crossguid")
      (license license:expat))))

;; Kodi requires using their own special forks of these libraries.
;; In addition, it insists on downloading and building these as part
;; of the standard build process. To make things easier, we bootstrap
;; and patch shebangs here, so we don't have to worry about it later.
(define libdvdnav/kodi
  (let ((commit "6.1.1"))
    (package
      (name "libdvdnav-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdnav")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "190wp4czs4594496vc6ifswg24wijd7c1z0whdkjdaf26rff5xha"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdnav")
      (synopsis (package-synopsis libdvdnav))
      (description (package-description libdvdnav))
      (license license:gpl2+))))

(define libdvdread/kodi
  (let ((commit "6.0.0-Leia-Alpha-3"))
    (package
      (name "libdvdread-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdread")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1xxn01mhkdnp10cqdr357wx77vyzfb5glqpqyg8m0skyi75aii59"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdread")
      (synopsis (package-synopsis libdvdread))
      (description (package-description libdvdread))
      (license (list license:gpl2+ license:lgpl2.1+)))))

(define libdvdcss/kodi
  (let ((commit "1.4.2-Leia-Beta-5"))
    (package
      (name "libdvdcss-bootstrapped")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xbmc/libdvdcss")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0j41ydzx0imaix069s3z07xqw9q95k7llh06fc27dcn6f7b8ydyl"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/xbmc/libdvdcss")
      (synopsis (package-synopsis libdvdcss))
      (description (package-description libdvdcss))
      (license license:gpl2+))))

(define-public fstrcmp
  (package
    (name "fstrcmp")
    (version "0.7.D001")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://fstrcmp.sourceforge.net/fstrcmp-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0xilghiy3mz78bjmfldi39qyy7jvw5b6wafsx370lw401y2qw0g4"))))
    (build-system gnu-build-system)
    (home-page "https://fstrcmp.sourceforge.net/")
    (arguments
     '(#:configure-flags '("SH=sh")))
    (native-inputs
     (list ghostscript ; ps2pdf
           groff libtool which))
    (synopsis "Fuzzy comparison of strings")
    (description
     "The fstrcmp project provides a library that is used to make fuzzy
comparisons of strings and byte arrays, including multi-byte character
strings.  This can be useful in error messages, enabling the suggestion of
likely valid alternatives.  In compilers, this can reduce the cascade of
secondary errors.")
    (license license:gpl3+)))

(define-public kodi
  (package
    (name "kodi")
    (version "19.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xbmc/xbmc")
                    (commit (string-append version "-Matrix"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pfd1ajivr865h0fkpbv778626c4czrvq8650bzqv9aqzh8f36my"))
              (patches (search-patches "kodi-set-libcurl-ssl-parameters.patch"
                                       "kodi-mesa-eglchromium.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (for-each delete-file-recursively
                            '("project/BuildDependencies/"
                              "tools/codegenerator/groovy/commons-lang-2.6.jar"
                              "tools/codegenerator/groovy/groovy-all-2.4.4.jar"
                              ;; Purge these sources:
                              ;; "tools/depend/native/JsonSchemaBuilder"
                              ;; "tools/depend/native/TexturePacker"
                              ;; "lib/libUPnP"
                              "lib/libUPnP/Neptune/ThirdParty"
                              "project/Win32BuildSetup/tools/7z"))))))
    (build-system cmake-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags
       (list "-DCORE_PLATFORM_NAME=x11"
             "-DAPP_RENDER_SYSTEM=gl"
             "-DENABLE_INTERNAL_FFMPEG=OFF"
             "-DENABLE_INTERNAL_CROSSGUID=OFF"
             (string-append "-Dlibdvdread_URL="
                            (assoc-ref %build-inputs "libdvdread-bootstrapped"))
             (string-append "-Dlibdvdnav_URL="
                            (assoc-ref %build-inputs "libdvdnav-bootstrapped"))
             (string-append "-Dlibdvdcss_URL="
                            (assoc-ref %build-inputs "libdvdcss-bootstrapped"))
             (string-append "-DSYSTEM_LDFLAGS=-Wl,-rpath="
                            (assoc-ref %build-inputs "curl") "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; The build system tries to bootstrap these bundled components
         ;; during the regular build phase, which causes serious issues
         ;; because there's no time for shebangs to be patched.  So, we
         ;; bootstrap it on our own instead.
         (add-after 'unpack 'bootstrap-bundled-software
           (lambda _
             (let ((dirs '("tools/depends/native/JsonSchemaBuilder/src")))
               (every (lambda (third-party)
                        (with-directory-excursion third-party
                          (invoke "autoreconf" "-vif")))
                      dirs))))
         (add-after 'bootstrap-bundled-software 'patch-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Run groovy executable directly.
             (substitute* "xbmc/interfaces/swig/CMakeLists.txt"
               (("COMMAND \\$\\{Java_JAVA_EXECUTABLE\\}")
                "COMMAND groovy")
               (("ARGS \\$\\{JAVA_OPEN_OPTS\\} -cp \"\\$\\{classpath\\}\" groovy.ui.GroovyMain")
                "ARGS -cp \"${classpath}\" ")
               (("classpath \\$\\{GROOVY_DIR\\}/groovy-all-\\$\\{GROOVY_VER\\}.jar")
                "classpath ")
               (("\\$\\{GROOVY_DIR\\}/commons-lang-\\$\\{COMMONS_VER\\}.jar")
                (search-input-file inputs "/share/java/commons-lang-2.6.jar"))
               (("^set\\(GROOVY_VER.*")
                (string-append "set(GROOVY_VER 3.0.5)\n")))

             ;; Prevent the build scripts from calling autoreconf in the
             ;; build stage.  Otherwise, it would undo the bootstrapping
             ;; and shebang patching that we worked so hard for.
             (substitute* "cmake/modules/FindLibDvd.cmake"
               ;; The libdvd* sources that we bootstrapped separately are
               ;; unpacked in the build phase. This is our best opportunity
               ;; to make them writable before the build process starts.
               (("autoreconf -vif") "chmod -R u+w ."))

             (substitute* "xbmc/platform/posix/PosixTimezone.cpp"
               (("/usr/share/zoneinfo")
                (search-input-directory inputs "share/zoneinfo")))

             ;; Don't phone home to check for updates.
             (substitute* "system/addon-manifest.xml"
               (("<addon optional=\\\"true\\\">service.xbmc.versioncheck</addon>")
                ""))

             ;; Let's disable some tests that are known not to work here.
             ;; Doing this later while in the cmake "../build" directory
             ;; is trickier.
             (substitute* "xbmc/utils/test/TestSystemInfo.cpp"
               (("TEST_F\\(TestSystemInfo, GetOsPrettyNameWithVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsPrettyNameWithVersion)")
               (("TEST_F\\(TestSystemInfo, GetOsName\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsName)")
               (("TEST_F\\(TestSystemInfo, GetOsVersion\\)")
                "TEST_F(TestSystemInfo, DISABLED_GetOsVersion)"))
             (substitute* "xbmc/utils/test/TestCPUInfo.cpp"
               (("TEST_F\\(TestCPUInfo, GetCPUFrequency\\)")
                "TEST_F(TestCPUInfo, DISABLED_GetCPUFrequency)"))
             (substitute* "xbmc/test/TestDateTime.cpp"
               (("TEST_F\\(TestDateTime, TmOperators\\)")
                "TEST_F(TestDateTime, DISABLED_TmOperators)")
               (("TEST_F\\(TestDateTime, GetAsTm\\)")
                "TEST_F(TestDateTime, DISABLED_GetAsTm)"))))
         (add-before 'build 'set-build-environment
           (lambda _
             ;; Some bundled build scripts fall back to /bin/sh
             ;; if this is not set.
             (setenv "CONFIG_SHELL" (which "sh"))))
         (add-before 'check 'build-kodi-test
           (lambda _
             (invoke "make" "kodi-test"))))))
    ;; TODO: Add dependencies for:
    ;; - cec
    ;; - plist
    ;; - shairplay
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           googletest
           groovy
           openjdk9                     ;like groovy
           java-commons-lang
           libdvdcss/kodi
           libdvdnav/kodi
           libdvdread/kodi
           libtool
           pkg-config
           swig
           yasm))
    (inputs
     (list alsa-lib
           avahi
           bluez
           crossguid
           curl
           dcadec
           dbus
           eudev
           ffmpeg-4
           flac
           flatbuffers
           fmt
           fontconfig
           freetype
           fribidi
           fstrcmp
           giflib
           glew
           gnutls
           lame
           lcms
           libass
           libbluray
           libcap
           libcdio
           libdrm
           libgcrypt
           libjpeg-turbo
           libltdl
           libmad
           libmicrohttpd
           libmpeg2
           libnfs
           libogg
           libpng
           libssh
           libtiff
           libva
           libvorbis
           libxcrypt
           libxml2
           libxrandr
           libxrender
           libxslt
           lzo
           (list mariadb "lib")
           (list mariadb "dev")
           openssl
           pcre
           pulseaudio
           python
           rapidjson
           samba
           spdlog
           sqlite
           taglib
           tinyxml
           tzdata
           util-linux
           zip
           zlib))
    (synopsis "Media center for home theater computers")
    (description "Kodi is a media center application for playing videos,
music, games, etc.  Kodi is highly customizable and features a theme and
plug-in system.")
    (home-page "https://kodi.tv")
    ;; XBMC is largely GPL2+, with some library components as LGPL2.1+, but
    ;; there are some other licenses spread throughout.
    (license (list license:gpl2+ license:lgpl2.1+
                   license:gpl3+         ;WiiRemote client
                   license:expat         ;cpluff, dbwrappers
                   license:public-domain ;cpluff/examples
                   license:bsd-3         ;misc
                   license:bsd-2))))     ;xbmc/freebsd

(define-public kodi/wayland
  (package
    (inherit kodi)
    (name "kodi-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments kodi)
       ((#:configure-flags flags)
        `(cons "-DCORE_PLATFORM_NAME=wayland"
               (delete "-DCORE_PLATFORM_NAME=x11" ,flags)))))
    (inputs
     (modify-inputs (package-inputs kodi)
       (prepend libinput
                libxkbcommon
                waylandpp
                wayland-protocols)))
    (synopsis "Kodi with Wayland rendering backend")))

(define-public kodi-cli
  (let ((commit "104dc23b2a993c8e6db8c46f4f8bec24b146549b") ; Add support for
        (revision "1"))                                     ; `$HOME/.kodirc'.
    (package
      (name "kodi-cli")
      (version (string-append "1.1-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference (url "https://github.com/nawar/kodi-cli")
                                    (commit commit)))
                (sha256
                 (base32
                  "1xjhasc5gngfxpr1dlzy6q24w0wpdfjx12p43fanjppxw4i49n5p"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system trivial-build-system)
      (inputs
       (list bash curl mps-youtube))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "kodi-cli"
             (("/bin/bash")
              (search-input-file %build-inputs "/bin/bash"))
             (("output=\\$\\((curl)" all curl)
              (string-append "output=$("
                             (assoc-ref %build-inputs "curl")
                             "/bin/" curl))
             (("play_youtube `(mpsyt)" all mpsyt)
              (string-append "play_youtube `"
                             (assoc-ref %build-inputs "mps-youtube")
                             "/bin/" mpsyt)))
           (install-file "kodi-cli" (string-append %output "/bin"))
           #t)))
      (home-page "https://github.com/nawar/kodi-cli")
      (synopsis "Control Kodi from the command line")
      (description "@code{kodi-cli} is a tool for sending commands to a Kodi
server using JSON RPC.

Features:

@itemize
@item Play, pause, stop the currently playing item.
@item Skip forward or backward in the currently playing item.
@item Play or queue to the currently list of YouTube videos.
@item Interactive and noninteractive volume control.
@item Interactive navigation.
@item Send text to the Kodi keyboard.
@item Toggle fullscreen.
@item Update or clean Kodi libraries.
@end itemize\n")
      (license license:gpl2+))))
