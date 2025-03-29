;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2022-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Remco van 't Veer <remco@remworks.net>
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

(define-module (gnu packages gstreamer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml))

(define-public openni2
  (package
    (name "openni2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/occipital/OpenNI2")
         (commit (string-append "v" version "-debian"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mfnyzpq53wnzgjfx91xcbx0nrl0lp1vrk1rk20a3gb3kshsr675"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("doxygen" ,doxygen)
       ("openjdk" ,openjdk14)
       ("openjdk:jdk" ,openjdk14 "jdk")
       ("python" ,python-wrapper)))
    (inputs
     `(("freeglut3" ,freeglut)
       ("libudev" ,eudev)
       ("libusb" ,libusb)))
    (synopsis "Framework for sensor-based 'Natural Interaction")
    (description "OpenNI is a framework for getting data to support
'Natural Interaction', i.e. skeleton tracking, gesture tracking, and similar
ways of getting data from humans.  It provides the interface for physical devices
and for middleware components.")
    (home-page "https://structure.io/openni")
    (license license:asl2.0)))

(define-public libdc1394
  (package
    (name "libdc1394")
    (version "2.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://sourceforge.net/projects/" name "/files/"
                              name "-2" "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1v8gq54n1pg8izn7s15yylwjf8r1l1dmzbm2yvf6pv2fmb4mz41b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen perl pkg-config))
    (inputs
     (list glu
           libraw1394
           libusb
           libxv
           mesa
           sdl
           v4l-utils-minimal))
    (synopsis "1394-Based Digital Camera Control Library")
    (description "LibDC1394 is a library that provides functionality to control
any camera that conforms to the 1394-Based Digital Camera Specification written
by the 1394 Trade Association.  It utilizes the lowlevel functionality provided
by libraw1394 to communicate with the camera.  It also uses the video1394 kernel
module for the DMA capture of the video flow.")
    (home-page "https://damien.douxchamps.net/ieee1394/libdc1394/")
    (license license:lgpl2.0+)))

(define-public ccextractor
  (package
    (name "ccextractor")
    (version "0.94")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CCExtractor/ccextractor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       ;; FIXME: Delete the 'src/thirdparty directory and unbundle the
       ;; libraries it contains, such as freetype, libpng, zlib, and others.
       (patches (search-patches "ccextractor-add-missing-header.patch"
                                "ccextractor-autoconf-tesseract.patch"
                                "ccextractor-fix-ocr.patch"))
       (sha256
        (base32 "1hrk4xlzkvk9pnv0yr4whcsh8h4fzk42mrf30dsr3xzh1lgpfslg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-ffmpeg"
                   "--enable-ocr"
                   "--enable-hardsubx"
                   ;; Disable Rust support, as there's no rust source included
                   ;; and cargo wants to fetch the crates from the network
                   ;; (see:
                   ;; https://github.com/CCExtractor/ccextractor/issues/1502).
                   "--without-rust")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "linux")))
                        (add-after 'chdir 'patch-pre-build.sh
                          (lambda _
                            (substitute* "pre-build.sh"
                              (("/usr/bin/env") (which "env")))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              ;; There is no test suite; simply run the binary
                              ;; to validate there are no obvious problems.
                              (invoke "./ccextractor" "--help")))))))
    (native-inputs (list autoconf automake pkg-config))
    (inputs (list ffmpeg-3.4 leptonica-1.80 tesseract-ocr-4))
    (synopsis "Closed Caption Extractor")
    (description "CCExtractor is a tool that analyzes video files and produces
independent subtitle files from the closed captions data.  It is portable, small,
and very fast.")
    (home-page "https://www.ccextractor.org/")
    (license license:gpl2+)))

(define-public libvisual
  (package
    (name "libvisual")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Libvisual/libvisual")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12xf0qzn9w090kakrj59makjbjg9vhga5mgchmx6c1ypw10fjfbc"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; The package is in a sub-dir of this repo.
               (add-after 'unpack 'chdir
                 (lambda _
                   (chdir "libvisual"))))))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           gettext-minimal
           intltool
           libtool
           pkg-config))
    (inputs
     (list sdl12-compat))
    (native-search-paths
     (list
      (search-path-specification
       (variable "LIBVISUAL_PLUGINS_BASE_DIR")
       (files '("lib/libvisual-0.4")))))
    ;; To load libvisual-plugins.
    (search-paths native-search-paths)
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license
     (list
      ;; Libraries.
      license:lgpl2.1+
      ;; Examples and Tests.
      license:gpl2+))))

(define-public libvisual-plugins
  (package
    (name "libvisual-plugins")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Libvisual/libvisual")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12xf0qzn9w090kakrj59makjbjg9vhga5mgchmx6c1ypw10fjfbc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-gstreamer-plugin"
                               "--disable-oinksie"
                               "--disable-corona"
                               "--disable-gforce"
                               (string-append "--with-plugins-base-dir="
                                              (assoc-ref %outputs "out")
                                              "/lib/libvisual-0.4"))
       #:phases (modify-phases %standard-phases
                  ;; The package is in a sub-dir of this repo.
                  (add-after 'unpack 'chdir
                    (lambda _
                      (chdir "libvisual-plugins"))))))
    (native-inputs
     (list automake
           autoconf
           autoconf-archive
           bison
           flex
           gettext-minimal
           intltool
           libtool
           pkg-config))
    (inputs
     (list alsa-lib
           esound
           (librsvg-for-system)
           gtk+
           jack-2
           libx11
           libxext))
    (propagated-inputs (list libvisual))
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license license:gpl2+)))

(define-public esound
  (package
    (name "esound")
    (version "0.2.41")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.gnome.org/Archive/esound.git")
         (commit "ESOUND_0_2_41")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141jg70fim276i8k2kyypm84gy89i1k9mm4yf68mfwnybvjw1d6n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           gnome-common
           libtool
           pkg-config
           tcsh ; for the tests
           which))
    (inputs
     (list alsa-lib pcaudiolib tcp-wrappers))
    (propagated-inputs
     (list audiofile))
    (synopsis "Enlightened Sound Daemon")
    (description "The Enlightened Sound Daemon mixes several audio streams for
playback by a single audio device.  You can also pre-load samples, and play them
back without having to send all the data for the sound.  Network transparency is
also built in, so you can play sounds on one machine, and listen to them on
another.")
    (home-page "https://web.archive.org/web/20160528230227/http://www.tux.org/~ricdude/overview.html")
    (license
     (list
      ;; Libraries.
      license:lgpl2.0+
      ;; Others.
      license:gpl2+))))

(define-public orc
  (package
    (name "orc")
    (version "0.4.40")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/data/src/"
                                  "orc/orc-" version ".tar.xz"))
              (sha256
               (base32
                "1avlxyn8nvpml5lzdqpa0zq7vnrqj731y1h5jvyl2z7vipkvxhiz"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-faulty-test
           (lambda _
             ;; XXX Disable the 'test-limits' and 'exec_opcodes_sys'
             ;; tests, which fail on some machines.  See:
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=735273
             (substitute* '("testsuite/test-limits.c"
                            "testsuite/exec_opcodes_sys.c")
               (("if \\(error\\) return 1;")
                "if (error) return 77;"))
             #t)))))
    (native-inputs
     (list gtk-doc/stable))
    (home-page "https://gstreamer.freedesktop.org/modules/orc.html")
    (synopsis "Oil runtime compiler")
    (description
     "Orc is a just-in-time compiler implemented as a library and set of
associated tools for compiling and executing simple programs that operate on
arrays of data.")
    ;; The source code implementing the Marsenne Twister algorithm is licensed
    ;; under the 3-clause BSD license, the rest is under 2-clause BSD license.
    (license (list license:bsd-2 license:bsd-3))))

(define-public gstreamer-docs
  (package
    (name "gstreamer-docs")
    (version "1.24.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gstreamer-docs"
                    "/gstreamer-docs-" version ".tar.xz"))
              (sha256
               (base32
                "0vb4d35rglvjlj2y9r0nlgankflqw0ql6gwsf4612505sa01mz7s"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules ((guix build utils)))
         (let* ((source (assoc-ref %build-inputs "source"))
                (tar (assoc-ref %build-inputs "tar"))
                (xz (assoc-ref %build-inputs "xz"))
                (out (assoc-ref %outputs "out"))
                (books (string-append out "/share/devhelp/books")))
           (setenv "PATH" (string-append xz "/bin"))
           (mkdir-p books)
           (with-directory-excursion books
             (invoke (string-append tar "/bin/tar") "-xvf" source
                     "--strip-components=3"
                     (string-append ,name "-" ,version
                                    "/devhelp/books/GStreamer")))))))
    (native-inputs
     (list tar xz))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Developer documentation for GStreamer")
    (description
     "This package contains manuals, tutorials, and API reference for
the GStreamer multimedia framework.")
    ;; The documentation is covered by multiple licenses.  Anything not
    ;; explicitly mentioned below is LGPL2.1+.  See README.md for details.
    (license (list
              ;; The tutorial code can be used with either of these licenses,
              ;; at the users option.
              license:lgpl2.1+ license:bsd-2 license:expat
              ;; The developer manual and plugin writer guide carries
              ;; the Open Publication License v1.0.
              (license:fsf-free "https://opencontent.org/openpub/"
                                "The Open Publication License v1.0")
              ;; Tutorials are covered by CC-BY-SA 4.0.
              license:cc-by-sa4.0))))

;; Increase the test timeouts to accommodate slow or busy machines.
(define %common-gstreamer-phases
  '((add-after 'unpack 'increase-test-timeout
      (lambda _
        (substitute* "tests/check/meson.build"
          (("'CK_DEFAULT_TIMEOUT', '[0-9]*'")
           "'CK_DEFAULT_TIMEOUT', '600'")
          (("timeout ?: .*\\)")
           "timeout: 90 * 60)"))))))

(define-public gstreamer
  (package
    (name "gstreamer")
    (version "1.24.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
             version ".tar.xz"))
       (sha256
        (base32
         "03r6rwmih3nqxrfw9nkhpbwpwp1yf7qw3m2phl6a027mxrmppx7b"))))
    (build-system meson-build-system)
    (arguments
     (list #:disallowed-references (list python)
           #:phases
           #~(modify-phases %standard-phases
               #$@%common-gstreamer-phases
               #$@(if (string-prefix? "i686" (or (%current-target-system)
                                                 (%current-system)))
                      ;; FIXME: These tests consistently fail in the Guix CI:
                      ;;   https://issues.guix.gnu.org/57868
                      '((add-after 'unpack 'disable-systemclock-test
                          (lambda _
                            (substitute* "tests/check/gst/gstsystemclock.c"
                              (("tcase_add_test \\(tc_chain, \
test_stress_cleanup_unschedule.*")
                               "")
                              (("tcase_add_test \\(tc_chain, \
test_stress_reschedule.*")
                               "")))))
                      '())
               (add-after 'patch-shebangs 'do-not-capture-python
                 (lambda _
                   ;; The patch-source-shebangs phase causes the following build
                   ;; script to reference Python in its shebang, which is
                   ;; unnecessary.
                   (substitute* (string-append
                                 #$output "/libexec/gstreamer-1.0/"
                                 "gst-plugins-doc-cache-generator")
                     (((which "python3"))
                      "/usr/bin/env python3")))))))
    (propagated-inputs
     ;; In gstreamer-1.0.pc:
     ;;   Requires: glib-2.0, gobject-2.0
     ;;   Requires.private: gmodule-no-export-2.0 libunwind libdw
     (list elfutils                     ;libdw
           glib libunwind))
    (native-inputs
     (list bash-completion
           bison flex
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           perl
           pkg-config
           python))
    (inputs
     (list gmp libcap
           ;; For tests.
           gsl))
    (native-search-paths
     (list (search-path-specification
            (variable "GST_PLUGIN_SYSTEM_PATH")
            (files '("lib/gstreamer-1.0")))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Multimedia framework")
    (description
     "GStreamer is a library for constructing graphs of media-handling
components.  The applications it supports range from simple Ogg/Vorbis
playback, audio/video streaming to complex audio mixing and video
non-linear editing.

Applications can take advantage of advances in codec and filter technology
transparently.  Developers can add new codecs and filters by writing a
simple plugin with a clean, generic interface.

This package provides the core library and elements.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-base
  (package
    (name "gst-plugins-base")
    (version "1.24.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gstreamer.freedesktop.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "1jspg24zlpmg4bxa298v6l8hcaqw27411dj2v16y0g3xj13bkcsv"))))
    (build-system meson-build-system)
    (propagated-inputs
     (list glib                     ;required by gstreamer-sdp-1.0.pc
           gstreamer                ;required by gstreamer-plugins-base-1.0.pc
           libgudev                 ;required by gstreamer-gl-1.0.pc
           ;; wayland-client.h is referred to in
           ;; include/gstreamer-1.0/gst/gl/wayland/gstgldisplay_wayland.h
           wayland
           orc))                    ;required by gstreamer-audio-1.0.pc
    (inputs
     ;; TODO: Add libvorbisidec
     (list alsa-lib
           cdparanoia
           graphene
           iso-codes/pinned
           libjpeg-turbo
           libogg
           libpng
           libtheora
           libvisual
           libvorbis
           libx11
           libxext
           libxv
           mesa
           opus
           pango
           wayland-protocols
           zlib))
    (native-inputs
     (list pkg-config
           `(,glib "bin")
           gobject-introspection
           python-wrapper
           gettext-minimal
           xorg-server-for-tests))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          #$@%common-gstreamer-phases
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (substitute* "tests/check/meson.build"
                ;; This test causes nondeterministic failures (see:
                ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-base/-/issues/950).
                ((".*'elements/appsrc.c'.*")
                 ""))
              ;; Some other tests fail on other architectures.
              #$@(cond
                   ((target-x86-32?)
                    #~((substitute* "tests/check/meson.build"
                         ((".*'libs/dsd\\.c'.*") "")
                         ((".*'libs/libsabi\\.c'.*") "")
                         ((".*'elements/volume\\.c'.*") ""))))
                   ((target-ppc64le?)
                    #~((substitute* "tests/check/meson.build"
                         ((".*'elements/glimagesink\\.c'.*") "")
                         ((".*'pipelines/gl-launch-lines\\.c'.*") ""))))
                   ((target-arm32?)
                    #~((substitute* "tests/check/meson.build"
                         ((".*'orc_adder.*") ""))))
                   ((target-riscv64?)
                    #~((substitute* "tests/check/meson.build"
                         ((".*'libs/gstglcolorconvert\\.c'.*") "")
                         ((".*'libs/gstglcontext\\.c'.*") "")
                         ((".*'libs/gstglmemory\\.c'.*") "")
                         ((".*'libs/gstglupload\\.c'.*") "")
                         ((".*'elements/glimagesink\\.c'.*") "")
                         ((".*'pipelines/gl-launch-lines\\.c'.*") "")
                         ((".*'elements/glstereo\\.c'.*") "")
                         ((".*'elements/glmixer\\.c'.*") ""))))
                   (else
                     #~()))))
          (add-before 'configure 'patch
            (lambda _
              (substitute* "tests/check/libs/pbutils.c"
                (("/bin/sh") (which "sh")))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis
     "Plugins for the GStreamer multimedia library")
    (description "This package provides an essential exemplary set of plug-ins
for the GStreamer multimedia library.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-good
  (package
    (name "gst-plugins-good")
    (version "1.24.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32 "17vr55pgh2paqi82l5jn841873c2w0lal7kgz2i3qzikzw5yazc9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t              ; To wrap binaries and/or compile schemas
      #:phases
      #~(modify-phases %standard-phases
          #$@%common-gstreamer-phases
          (add-after 'unpack 'absolutize-libsoup-library
            (lambda* (#:key inputs #:allow-other-keys)
              (define libsoup
                (search-input-file inputs "lib/libsoup-3.0.so"))

              (substitute* "ext/soup/gstsouploader.c"
                (("(#define LIBSOUP_3_SONAME ).+$" _ prefix)
                 (string-append prefix "\"" libsoup "\"\n")))))
          (add-after 'unpack 'skip-failing-tests
            (lambda _
              (substitute* "tests/check/elements/flvmux.c"
                ;; This test randomly times out (see:
                ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/786).
                ((".*tcase_add_test.*test_video_caps_late.*")
                 ""))
              (substitute* "tests/check/meson.build"
                ;; Reported as shaky upstream, see
                ;; <https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/785>
                (("\\[ 'elements/flvmux' \\]") "[ 'elements/flvmux', true ]"))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gsettings-desktop-schemas
           libxml2
           perl
           pkg-config
           python-wrapper
           xorg-server-for-tests))
    (inputs
     (list aalib
           bzip2
           cairo
           flac
           (librsvg-for-system)
           glib
           glib-networking
           glu
           gtk+
           jack-2
           lame
           libavc1394
           libcaca
           libdv
           libgudev
           libiec61883
           libjpeg-turbo
           libpng
           libshout
           libsoup
           libvpx
           libx11
           libxdamage
           libxfixes
           libxext
           libxshmfence
           mesa
           mpg123
           orc
           pulseaudio
           speex
           taglib
           twolame
           v4l-utils-minimal
           wavpack
           zlib))
    (propagated-inputs
     (list gstreamer gst-plugins-base))
    (synopsis "GStreamer plugins and helper libraries")
    (description "GStreamer-Plugins-Good is a collection of plug-ins you'd want
to have right next to you on the battlefield.  Shooting sharp and making no
mistakes, these plug-ins have it all: good looks, good code, and good
licensing.  Documented and dressed up in tests.  If you're looking for a role
model to base your own plug-in on, here it is.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-plugins-good-qt
  (package
    (inherit gst-plugins-good)
    (name "gst-plugins-good-qt")
    (inputs
     (modify-inputs (package-inputs gst-plugins-good)
       (prepend qtbase-5
                qtdeclarative-5
                qtwayland-5
                qtx11extras)))))

(define-public gst-plugins-bad
  (package
    (name "gst-plugins-bad")
    (version "1.24.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/src/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1aswb97v1ird3rmfcsa32bvp4kgp8r987f83pd1knx8amylzgz1n"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled copy of usrsctp.
                  (delete-file-recursively "ext/sctp/usrsctp")))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dsctp-internal-usrsctp=disabled"
                                ;; TODO: Figure out why audiovisualizer test
                                ;; fails on riscv64-linux.
                                #$@(if (target-riscv64?)
                                       #~("-Daudiovisualizers=disabled")
                                       #~()))
      #:glib-or-gtk? #t              ; To wrap binaries and/or compile schemas
      #:phases
      #~(modify-phases %standard-phases
          #$@%common-gstreamer-phases
          (add-after 'unpack 'adjust-tests
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (let ((gst-plugins-good (assoc-ref (or native-inputs inputs)
                                                 "gst-plugins-good")))
                (substitute* "tests/check/meson.build"
                  ;; Make gst-plugin-good available for tests, see
                  ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/1426
                  (("'GST_PLUGIN_SYSTEM_PATH_1_0', ''")
                   (string-append "'GST_PLUGIN_SYSTEM_PATH_1_0', '"
                                  gst-plugins-good "/lib/gstreamer-1.0'"))

                  ;; The 'elements_shm.test_shm_live' test sometimes times out
                  ;; (see:
                  ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/790).
                  ((".*'elements/shm\\.c'.*") "")

                  ;; The 'elements_curlhttpsrc' test sometimes times out.
                  ((".*'elements/curlhttpsrc\\.c'.*") "")

                  ;; The 'mxfdemux' test fails for unknown reasons (see:
                  ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/3921).
                  ((".*'elements/mxfdemux.c'.*") "")

                  ;; Unexpected critical/warning, see
                  ;; <https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/3000>
                  ((".*'elements/netsim\\.c'.*") "")

                  ;; TODO: Figure out why this test fails on riscv64-linux.
                  #$@(if (target-riscv64?)
                         `((("'elements/viewfinderbin\\.c'\\].*],")
                            "'elements/viewfinderbin.c'], true, ],"))
                         '())

                  ;; This test is flaky on at least some architectures.
                  ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/1244
                  #$@(if (member (%current-system)
                                 '("aarch64-linux" "riscv64-linux"))
                         `((("'elements/camerabin\\.c'\\].*],")
                            "'elements/camerabin.c'], true, ],"))
                         '())

                  ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/1412
                  ((".*elements/dtls\\.c.*") "")

                  ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/3921
                  ((".*elements/mxfdemux\\.c.*") ""))
                (substitute* "tests/check/elements/zxing.c"
                  ;; zxing 1.2.0 seemingly changed the type representation of
                  ;; the EAN_13 structure; disable it.
                  ((".*\"EAN_13\".*")
                   "")))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (propagated-inputs
     (list gstreamer gst-plugins-base))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-mkenums, etc.
           gobject-introspection
           gsettings-desktop-schemas
           gst-plugins-good             ;for tests
           perl
           pkg-config
           python-wrapper
           xorg-server-for-tests))
    (inputs
     (append
      (if (target-x86?) (list mediasdk) '())
      ;; Note: svt-hevc cannot be used, as it would break the package for
      ;; older x86_64 CPUs that lack AVX2, such as the Core 2 Duo (see:
      ;; https://github.com/OpenVisualCloud/SVT-HEVC/issues/573#issuecomment-680678144).
      (list bluez
            bzip2
            cairo
            ;; ccextractor
            chromaprint
            curl
            directfb
            ;; dssim
            faad2
            flite
            fluidsynth
            glib
            glib-networking
            glu
            gsm
            gtk+
            iqa
            ladspa
            lcms
            libaom
            libass
            libbs2b
            libdc1394
            libdca
            libde265
            libdrm
            libdvdnav
            libdvdread
            libexif
            libfdk
            libgcrypt
            libgme
            libgudev
            libkate
            libmms
            libmodplug
            libmpcdec
            libnice
            libofa
            libopenmpt
            (librsvg-for-system)
            libsndfile
            libsrtp
            libssh2
            libtiff
            libusb
            libva
            libvdpau
            libwebp
            libx11
            libxcb
            libxext
            libxkbcommon
            libxml2
            libxshmfence
            lilv
            lrdf
            lv2
            mesa
            mjpegtools
            neon
            nettle
            openal
            ;; opencv
            openexr
            openh264
            openjpeg
            ;; openni2
            opensles
            openssl-1.1
            opus
            orc
            pango
            rtmpdump
            sbc
            lksctp-tools
            soundtouch
            spandsp
            srt
            tinyalsa
            transcode
            usrsctp
            v4l-utils-minimal
            vo-aacenc
            vo-amrwbenc
            vulkan-headers
            vulkan-loader
            x265
            wayland
            webrtc-audio-processing
            wildmidi
            wpebackend-fdo
            zbar-minimal
            zxing-cpp-1.2)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Plugins for the GStreamer multimedia library")
    (description
     "GStreamer Bad Plug-ins is a set of plug-ins whose quality aren't up to
par compared to the rest.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-ugly
  (package
    (name "gst-plugins-ugly")
    (version "1.24.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gstreamer.freedesktop.org/src/"
                       name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1dn33ivfc0rnn545y40303h5z9bm5ca9f8j2czmhbk9q1w8k0ssb"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t         ; To wrap binaries and/or compile schemas
           #:phases
           #~(modify-phases %standard-phases
               #$@%common-gstreamer-phases
               (add-before 'check 'pre-check
                 (lambda _
                   ;; Tests require a running X server.
                   (system "Xvfb :1 +extension GLX &")
                   (setenv "DISPLAY" ":1")
                   ;; Tests write to $HOME.
                   (setenv "HOME" (getcwd))
                   ;; Tests look for $XDG_RUNTIME_DIR.
                   (setenv "XDG_RUNTIME_DIR" (getcwd))
                   ;; For missing '/etc/machine-id'.
                   (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gsettings-desktop-schemas
           perl
           pkg-config
           python-wrapper
           xorg-server-for-tests))
    (inputs
     (list glib
           glib-networking
           liba52
           libcdio
           libdvdread
           libmpeg2
           libx264
           opencore-amr
           orc))
    (propagated-inputs
     (list gstreamer gst-plugins-base))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Plugins-Ugly are the ones that might have a patent noose
around their neck, or a lock-up license, or any other problem that makes you
think twice about shipping them.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-libav
  (package
    (name "gst-libav")
    (version "1.24.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32 "0v253lbic7abc9vpb690f80arql10193ljqkzgs03vh8wnd2ws1j"))))
    (build-system meson-build-system)
    (native-inputs (list perl pkg-config python-wrapper ruby))
    (inputs (list ffmpeg))
    (propagated-inputs (list gstreamer gst-plugins-base))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Libav contains a GStreamer plugin for using the encoders,
decoders, muxers, and demuxers provided by FFmpeg.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-editing-services
  (package
    (name "gst-editing-services")
    (version "1.24.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/" name "/"
                    "gst-editing-services-" version ".tar.xz"))
              (sha256
               (base32
                "04khlbw3wy5zr2s246252zrd4hnzv2x36l5avz0lxif6pr9nwl07"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; Most of the tests fail (see:
      ;; https://gitlab.freedesktop.org/gstreamer/gstreamer/-/issues/2489).
      #:tests? #f
      #:glib-or-gtk? #t              ; To wrap binaries and/or compile schemas
      #:phases #~(modify-phases %standard-phases
                   #$@%common-gstreamer-phases)))
    (propagated-inputs
     (list gstreamer gst-plugins-base))
    (inputs
     (list glib glib-networking gtk+ libxml2))
    (native-inputs
     (list flex
           gobject-introspection
           `(,glib "bin")
           gst-plugins-bad
           gst-plugins-good
           perl
           pkg-config
           python-wrapper))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer library for non-linear editors")
    (description
     "This is a high-level library for facilitating the creation of audio/video
non-linear editors.")
    (license license:gpl2+)))

(define-public gst-plugins/selection
  (lambda* (pkg #:key plugins configure-flags)
    "Build PKG with only PLUGINS enabled.  Optionally, if CONFIGURE-FLAGS are
given, also pass them to the build system instead of the ones used by PKG."
    (package/inherit pkg
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:configure-flags flags #~'())
         #~(append
            (list
             #$@(map (lambda (plugin)
                       (string-append "-D" plugin "=enabled"))
                     plugins))
            #$(or configure-flags flags)))
         ((#:phases phases)
           #~(modify-phases #$phases
               (add-after 'unpack 'disable-auto-plugins
                 (lambda _
                   (substitute* "meson_options.txt"
                     (("'auto'") "'disabled'")))))))))))

(define-public gst-plugins-bad-minimal
  (package
    (inherit (gst-plugins/selection gst-plugins-bad #:plugins '()))
    (name "gst-plugins-bad-minimal")
    (description "This package provides the smallest selection of GStreamer's
\"bad\" plugin set, essentially containing libraries and the gst-transcoder
binary, but none of the actual plugins.")))

(define-public python-gst
  (package
    (name "python-gst")
    (version "1.24.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gst-python/"
                    "gst-python-" version ".tar.xz"))
              (sha256
               (base32
                "0bplhfnvsi3l9fmfb346n2dvzi1jrxqpp4kcwiwsrjrlgic1vrl0"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules `((guix build meson-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:configure-flags
      #~(list (string-append
               "-Dpygi-overrides-dir="
               (python:site-packages %build-inputs %outputs) "/gi/overrides"))))
    (native-inputs
     (list pkg-config python))
    (propagated-inputs
     (list gst-plugins-base python-pygobject))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer GObject Introspection overrides for Python")
    (description
     "This package contains GObject Introspection overrides for Python that can
be used by Python applications using GStreamer.")
    (license license:lgpl2.1+)
    (properties `((upstream-name . "gst-python")))))

(define-public gst123
  (package
    (name "gst123")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://space.twc.de/~stefan/gst123/gst123-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0zaa117n4wkya9p903vkj8hj58lmdb66pxsdx5wwcv7nffbp5d67"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 ncurses gstreamer gst-plugins-base))
    (native-inputs
     (list pkg-config))
    (home-page "https://space.twc.de/~stefan/gst123.php")
    (synopsis "Flexible command line media player based on gstreamer")
    (description "The program gst123 is designed to be a more flexible command
line player in the spirit of ogg123 and mpg123, based on the gstreamer media
framework.  It plays all file formats gstreamer supports, so if you have a
music collection which contains different file formats, like flac, ogg and
mp3, you can use gst123 to play all your music files.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-espeak
  (package
    (name "gst-plugins-espeak")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/gst-plugins-espeak")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c9j5gbsl0rzwkqgg1c1a2vlyvcsliyl8a57qkwsqrnqrwnvw28w"))))
    (build-system gnu-build-system)
    (inputs (list espeak-ng gstreamer gst-plugins-base))
    (native-inputs (list autoconf automake libtool pkg-config))
    (home-page "http://wiki.sugarlabs.org/go/Activity_Team/gst-plugins-espeak")
    (synopsis "Gstreamer plugin for Espeak")
    (description "This is a Gstreamer @code{src} plugin to use the espeak
speech synthesizer as a sound source.")
    (license license:lgpl2.0+)))
