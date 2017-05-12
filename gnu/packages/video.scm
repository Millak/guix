;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Kei Kebreau <kei@openmailbox.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (gnu packages video)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages zip))

(define-public aalib
  (package
    (name "aalib")
    (version "1.4rc5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aa-project/aa-lib/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vkh19gb76agvh4h87ysbrgy82hrw88lnsvhynjf4vng629dmpgv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("makeinfo" ,texinfo)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key build inputs outputs #:allow-other-keys)
                    ;; This old `configure' script doesn't support
                    ;; variables passed as arguments.
                    (let ((out     (assoc-ref outputs "out"))
                          (ncurses (assoc-ref inputs "ncurses")))
                      (setenv "CONFIG_SHELL" (which "bash"))
                      (zero? (system* "./configure"
                                      (string-append "--prefix=" out)
                                      (string-append "--build=" build)
                                      ;; The ancient config.guess is unable to
                                      ;; guess the host triplet on mips64el.
                                      ,@(if (string=? "mips64el-linux"
                                                      (%current-system))
                                            '("--host=mips64el-unknown-linux-gnu")
                                            '())
                                      ;; The same is also true with aarch64.
                                      ,@(if (string=? "aarch64-linux"
                                                      (%current-system))
                                            '("--host=aarch64-unknown-linux-gnu")
                                            '())
                                      (string-append "--with-ncurses="
                                                     ncurses)))))))))
    (home-page "http://aa-project.sourceforge.net/aalib/")
    (synopsis "ASCII-art library")
    (description
     "AA-lib is a low level gfx library which does not require graphics device.
In fact, there is no graphical output possible.  AA-lib replaces those
old-fashioned output methods with powerful ascii-art renderer.")
    (license license:lgpl2.0+)))

(define-public liba52
  (package
    (name "liba52")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    ;; A mirror://sourceforge URI doesn't work, presumably
                    ;; because the SourceForge project is misconfigured.
                    "http://liba52.sourceforge.net/files/a52dec-" version
                    ".tar.gz"))
              (sha256
               (base32
                "0czccp4fcpf2ykp16xcrzdfmnircz1ynhls334q374xknd5747d2"))
              (patches (search-patches "liba52-enable-pic.patch"
                                       "liba52-set-soname.patch"
                                       "liba52-use-mtune-not-mcpu.patch"
                                       "liba52-link-with-libm.patch"))))
    (build-system gnu-build-system)
    ;; XXX We need to run ./bootstrap because of the build system fixes above.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments `(#:configure-flags '("--enable-shared")
                 #:phases
                 (modify-phases %standard-phases
                   ;; XXX We need to run ./bootstrap because of the build
                   ;; system fixes above.
                   (add-after
                    'unpack 'bootstrap
                    (lambda _ (zero? (system* "sh" "bootstrap")))))))
    (home-page "http://liba52.sourceforge.net/")
    (synopsis "ATSC A/52 stream decoder")
    (description "liba52 is a library for decoding ATSC A/52 streams.  The
A/52 standard is used in a variety of applications, including digital
television and DVD.  It is also known as AC-3.")
    (license license:gpl2+)))

(define-public libmpeg2
  (package
    (name "libmpeg2")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              ;; A mirror://sourceforge URI doesn't work, presumably
              ;; because the SourceForge project is misconfigured.
              (uri (string-append "http://libmpeg2.sourceforge.net/files/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m3i322n2fwgrvbs1yck7g5md1dbg22bhq5xdqmjpz5m7j4jxqny"))))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxv" ,libxv)
       ("libsm" ,libsm)
       ("libice" ,libice)
       ("sdl" ,sdl)))
    (build-system gnu-build-system)
    (home-page "http://libmpeg2.sourceforge.net/")
    (synopsis "MPEG1 and MPEG2 video decoder library")
    (description
     "libmpeg2 is a library which can decode MPEG1 and MPEG2 video streams.")
    (license license:gpl2+)))

(define-public libx264
  (package
    (name "libx264")
    (version "20170316-2245")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/pub/x264/snapshots/"
                                  "x264-snapshot-" version ".tar.bz2"))
              (sha256
               (base32
                "1s1nnvl3axz38sv4g09skijl6k9mbbngbb1nsh26w4dr2w2gyzad"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("yasm" ,yasm)))
    ;; TODO: Add gpac input
    (arguments
     `(#:tests? #f  ;no check target
       #:configure-flags '("--enable-shared"
                           ;; Don't build the command-line program.  If we
                           ;; want it later, we should do so in a different
                           ;; package to avoid a circular dependency (the x264
                           ;; program depends on ffmpeg and ffmpeg depends on
                           ;; libx264).
                           "--disable-cli"

                           ;; On MIPS, we must pass "--disable-asm" or else
                           ;; configure fails after printing: "You specified a
                           ;; pre-MSA CPU in your CFLAGS. If you really want
                           ;; to run on such a CPU, configure with
                           ;; --disable-asm."
                           ,@(if (string-prefix? "mips"
                                                 (or (%current-target-system)
                                                     (%current-system)))
                                 '("--disable-asm")
                                 '()))))
    (home-page "https://www.videolan.org/developers/x264.html")
    (synopsis "H.264 video coding library")
    (description "libx264 is an advanced encoding library for creating
H.264 (MPEG-4 AVC) video streams.")
    (license (list license:gpl2+         ;most files
                   license:isc           ;common/x86/x86inc.asm
                   license:lgpl2.1+      ;extras/getopt.c
                   license:bsd-3         ;extras/inttypes.h
                   (license:non-copyleft ;extras/cl*.h
                    "file://extras/cl.h"
                    "See extras/cl.h in the distribution.")))))

(define-public x265
  (package
    (name "x265")
    (version "2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.videolan.org/videolan/x265/"
                            "x265_" version ".tar.gz"))
        (sha256
         (base32
          "07z4ydxg0lk6j43h0wlh2xddb91cy4y4mny2ln71d4278b1hllj7"))
        (modules '((guix build utils)))
        (snippet
         '(delete-file-recursively "source/compat/getopt"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; tests are skipped if cpu-optimized code isn't built
       ;; Currently the source code doesn't check for aarch64
       ,@(if (string-prefix? "aarch64" (or (%current-target-system) (%current-system)))
           '(#:configure-flags '("-DENABLE_PIC=TRUE"))
           '())
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'prepare-build
           (lambda _
             (delete-file-recursively "build")
             (chdir "source")
             #t)))))
    (home-page "http://x265.org/")
    (synopsis "Library for encoding h.265/HEVC video streams")
    (description "x265 is a H.265 / HEVC video encoder application library,
designed to encode video or images into an H.265 / HEVC encoded bitstream.")
    (license license:gpl2+)))

(define-public libass
  (package
    (name "libass")
    (version "0.13.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libass/libass/releases/download/"
                    version "/libass-" version ".tar.xz"))
              (sha256
               (base32
                "0b9cj5xfsa7zwlk5m146fhv102v51iqs3rapq0n2xrz30k8p9a7q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("yasm" ,yasm)))
    (propagated-inputs
     `(("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("fontconfig" ,fontconfig)
       ("harfbuzz" ,harfbuzz)
       ("enca" ,enca)))
    (home-page "https://github.com/libass/libass")
    (synopsis "Subtitle rendering library for the ASS/SSA format")
    (description "libass is a subtitle rendering library for the
ASS/SSA (Advanced Substation Alpha/SubStation Alpha) subtitle format.")
    (license license:isc)))

(define-public libcaca
  (package
    (name "libcaca")
    (version "0.99.beta19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://caca.zoy.org/files/libcaca/libcaca-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x3j6yfyxl52adgnabycr0n38j9hx2j74la0hz0n8cnh9ry4d2qj"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freeglut" ,freeglut)
       ("ftgl" ,ftgl)
       ("imlib2" ,imlib2)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("zlib" ,zlib)))
    (home-page "http://caca.zoy.org/wiki/libcaca")
    (synopsis "Colour ASCII-art library")
    (description "libcaca is a graphics library that outputs text instead of
pixels, so that it can work on older video cards or text terminals.  It
supports Unicode, 2048 colors, dithering of color images, and advanced text
canvas operations.")
    (license (license:fsf-free "file://COPYING")))) ;WTFPL version 2

(define-public libdca
  (package
    (name "libdca")
    (version "0.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.videolan.org/pub/videolan/libdca/"
                    version "/libdca-" version ".tar.bz2"))
              (sha256
               (base32
                "0hh6a7l8vvccsd5i1fkv9av2gzv9fy8m0b8jpsn5p6hh4bh2586v"))))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libdca.html")
    (synopsis "DTS Coherent Acoustics decoder")
    (description "libdca is a library for decoding DTS Coherent Acoustics
streams.")
    (license license:gpl2+)))

(define-public libdv
  (package
    (name "libdv")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/" name "/" name "/"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fl96f2xh2slkv1i1ix7kqk576a0ak1d33cylm0mbhm96d0761d3"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libxv" ,libxv)))
    (home-page "http://libdv.sourceforge.net/")
    (synopsis "DV video (IEC 61834 and SMPTE 314M) codec")
    (description "The Quasar DV codec (libdv) is a software codec for DV
video, the encoding format used by most digital camcorders, typically those
that support the IEEE 1394 (a.k.a. FireWire or i.Link) interface.  Libdv was
developed according to the official standards for DV video: IEC 61834 and
SMPTE 314M.")
    (license license:lgpl2.1+)))

(define-public libva
  (package
    (name "libva")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.freedesktop.org/software/vaapi/releases/libva/libva-"
             version".tar.bz2"))
       (sha256
        (base32 "0wswfznj93jpnxhc3jwdk5j3pmyki0rs6k9vk4vnzds0dddximf1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("mesa" ,mesa)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'build 'fix-dlopen-paths
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "va/drm/va_drm_auth_x11.c"
                (("\"libva-x11\\.so\\.%d\"")
                 (string-append "\"" out "/lib/libva-x11.so.%d\"")))))))
       ;; Most drivers are in mesa's $prefix/lib/dri, so use that.  (Can be
       ;; overridden at run-time via LIBVA_DRIVERS_PATH.)
       #:configure-flags
       (list (string-append "--with-drivers-path="
                            (assoc-ref %build-inputs "mesa") "/lib/dri"))
       ;; However, we can't write to mesa's store directory, so override the
       ;; following make variable to install the dummy driver to libva's
       ;; $prefix/lib/dri directory.
       #:make-flags
       (list (string-append "dummy_drv_video_ladir="
                            (assoc-ref %outputs "out") "/lib/dri"))))
    (home-page "https://www.freedesktop.org/wiki/Software/vaapi/")
    (synopsis "Video acceleration library")
    (description "The main motivation for VA-API (Video Acceleration API) is
to enable hardware accelerated video decode/encode at various
entry-points (VLD, IDCT, Motion Compensation etc.) for prevailing coding
standards (MPEG-2, MPEG-4 ASP/H.263, MPEG-4 AVC/H.264, and VC-1/VMW3).")
    (license license:expat)))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "3.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "17anx7rnbi63if1ndr61836lf76dpn47n0y424hc48bj05y7z7jr"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gnutls" ,gnutls)
       ("opus" ,opus)
       ("ladspa" ,ladspa)
       ("lame" ,lame)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libtheora" ,libtheora)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libx11" ,libx11)
       ("libx264" ,libx264)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl2)
       ("soxr" ,soxr)
       ("speex" ,speex)
       ("twolame" ,twolame)
       ("x265" ,x265)
       ("xvid" ,xvid)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bc" ,bc)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("python" ,python-2) ; scripts use interpreter python2
       ("speex" ,speex)
       ("yasm" ,yasm)))
    (arguments
     `(#:test-target "fate"
       #:configure-flags
       ;; possible additional inputs:
       ;;   --enable-avisynth        enable reading of AviSynth script
       ;;                            files [no]
       ;;   --enable-frei0r          enable frei0r video filtering
       ;;   --enable-libaacplus      enable AAC+ encoding via libaacplus [no]
       ;;   --enable-libcelt         enable CELT decoding via libcelt [no]
       ;;   --enable-libdc1394       enable IIDC-1394 grabbing using libdc1394
       ;;                            and libraw1394 [no]
       ;;   --enable-libfaac         enable AAC encoding via libfaac [no]
       ;;   --enable-libfdk-aac      enable AAC de/encoding via libfdk-aac [no]
       ;;   --enable-libflite        enable flite (voice synthesis) support via
       ;;                            libflite [no]
       ;;   --enable-libgme          enable Game Music Emu via libgme [no]
       ;;   --enable-libgsm          enable GSM de/encoding via libgsm [no]
       ;;   --enable-libiec61883     enable iec61883 via libiec61883 [no]
       ;;   --enable-libilbc         enable iLBC de/encoding via libilbc [no]
       ;;   --enable-libmodplug      enable ModPlug via libmodplug [no]
       ;;   --enable-libnut          enable NUT (de)muxing via libnut,
       ;;                            native (de)muxer exists [no]
       ;;   --enable-libopencore-amrnb    enable AMR-NB de/encoding via
       ;;                                 libopencore-amrnb [no]
       ;;   --enable-libopencore-amrwb    enable AMR-WB decoding via
       ;;                                 libopencore-amrwb [no]
       ;;   --enable-libopencv       enable video filtering via libopencv [no]
       ;;   --enable-libopenjpeg     enable JPEG 2000 de/encoding via
       ;;                            OpenJPEG [no]
       ;;   --enable-librtmp         enable RTMP[E] support via librtmp [no]
       ;;   --enable-libschroedinger enable Dirac de/encoding via
       ;;                            libschroedinger [no]
       ;;   --enable-libshine        enable fixed-point MP3 encoding via
       ;;                            libshine [no]
       ;;   --enable-libssh          enable SFTP protocol via libssh [no]
       ;;                            (libssh2 does not work)
       ;;   --enable-libstagefright-h264  enable H.264 decoding via
       ;;                                 libstagefright [no]
       ;;   --enable-libutvideo      enable Ut Video encoding and decoding via
       ;;                            libutvideo [no]
       ;;   --enable-libv4l2         enable libv4l2/v4l-utils [no]
       ;;   --enable-libvidstab      enable video stabilization using
       ;;                            vid.stab [no]
       ;;   --enable-libvo-aacenc    enable AAC encoding via libvo-aacenc [no]
       ;;   --enable-libvo-amrwbenc  enable AMR-WB encoding via
       ;;                            libvo-amrwbenc [no]
       ;;   --enable-libwavpack      enable wavpack encoding via libwavpack [no]
       ;;   --enable-libxavs         enable AVS encoding via xavs [no]
       ;;   --enable-libzmq          enable message passing via libzmq [no]
       ;;   --enable-libzvbi         enable teletext support via libzvbi [no]
       ;;   --enable-opencl          enable OpenCL code
       '("--enable-avresample"
         "--enable-gpl" ; enable optional gpl licensed parts
         "--enable-shared"
         "--enable-fontconfig"
         "--enable-gnutls"
         "--enable-ladspa"
         "--enable-libass"
         "--enable-libbluray"
         "--enable-libcaca"
         "--enable-libcdio"
         "--enable-libfreetype"
         "--enable-libmp3lame"
         "--enable-libopus"
         "--enable-libpulse"
         "--enable-libsoxr"
         "--enable-libspeex"
         "--enable-libtheora"
         "--enable-libtwolame"
         "--enable-libvorbis"
         "--enable-libvpx"
         "--enable-libxvid"
         "--enable-libx264"
         "--enable-libx265"
         "--enable-openal"
         "--enable-opengl"

         "--enable-runtime-cpudetect"

         ;; Runtime cpu detection is not implemented on
         ;; MIPS, so we disable some features.
         "--disable-mips32r2"
         "--disable-mipsdsp"
         "--disable-mipsdspr2"
         "--disable-mipsfpu")
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (zero? (apply system*
                            "./configure"
                            (string-append "--prefix=" out)
                            ;; Add $libdir to the RUNPATH of all the binaries.
                            (string-append "--extra-ldflags=-Wl,-rpath="
                                           out "/lib")
                            configure-flags)))))
         (add-before
          'check 'set-ld-library-path
          (lambda _
            ;; Allow $(top_builddir)/ffmpeg to find its dependencies when
            ;; running tests.
            (let* ((dso  (find-files "." "\\.so$"))
                   (path (string-join (map dirname dso) ":")))
              (format #t "setting LD_LIBRARY_PATH to ~s~%" path)
              (setenv "LD_LIBRARY_PATH" path)
              #t))))))
    (home-page "https://www.ffmpeg.org/")
    (synopsis "Audio and video framework")
    (description "FFmpeg is a complete, cross-platform solution to record,
convert and stream audio and video.  It includes the libavcodec
audio/video codec library.")
    (license license:gpl2+)))

(define-public ffmpeg-2.8
  (package
    (inherit ffmpeg)
    (version "2.8.11")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rhz7rhmhhh8bjcj3dc82haisn3qjbzqlr7k6d6v7064jgn3maiq"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        `(map (lambda (flag)
                (if (string=? flag "--disable-mipsdsp")
                    "--disable-mipsdspr1"
                    flag))
              ,flags))))))

(define-public vlc
  (package
    (name "vlc")
    (version "2.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://download.videolan.org/pub/videolan/vlc/"
                   version "/vlc-" version ".tar.xz"))
             (sha256
              (base32
               "1gjkrwlg8ab3skzl67cxb9qzg4187ifckd1z9kpy11q058fyjchn"))
             (modules '((guix build utils)))
             (snippet
              ;; There are two occurrences where __DATE__ and __TIME__ are
              ;; used to capture the build time and show it to the user.
              '(substitute* (find-files "." "help\\.c(pp)?$")
                 (("__DATE__") "\"2016\"")
                 (("__TIME__") "\"00:00\"")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("git" ,git) ; needed for a test
       ("pkg-config" ,pkg-config)))
    ;; FIXME: Add optional inputs once available.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("avahi" ,avahi)
       ("dbus" ,dbus)
       ("flac" ,flac)
       ("ffmpeg" ,ffmpeg-2.8)               ;fails to build against ffmpeg 3.0
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gnutls" ,gnutls)
       ("liba52" ,liba52)
       ("libcddb" ,libcddb)
       ("libgcrypt" ,libgcrypt)
       ("libkate" ,libkate)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libsamplerate" ,libsamplerate)
       ("libssh2" ,libssh2)
       ("libvorbis" ,libvorbis)
       ("libtheora" ,libtheora)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxml2" ,libxml2)
       ("libxpm" ,libxpm)
       ("lua" ,lua-5.1)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("qt" ,qt) ; FIXME: reenable modular qt after update - requires building
       ;("qtbase" ,qtbase) with -std=gnu++11.
       ;("qtx11extras" ,qtx11extras)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("speex" ,speex)
       ("x265" ,x265)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:configure-flags
       `(;; Gross workaround for <https://trac.videolan.org/vlc/ticket/16907>.
         ;; In our case, this led to a test failure:
         ;;   test_libvlc_equalizer: libvlc/equalizer.c:122: test_equalizer: Assertion `isnan(libvlc_audio_equalizer_get_amp_at_index (equalizer, u_bands))' failed.
         "ac_cv_c_fast_math=no"

         ,(string-append "LDFLAGS=-Wl,-rpath -Wl,"
                         (assoc-ref %build-inputs "ffmpeg")
                         "/lib"))                 ;needed for the tests

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-visual-tests
           ;; Some of the tests require using the display to test out VLC,
           ;; which fails in our sandboxed build system
           (lambda _
             (substitute* "test/run_vlc.sh"
                          (("./vlc --ignore-config") "echo"))
             #t))
         (add-after 'install 'regenerate-plugin-cache
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'install-exec-hook' rule in the top-level Makefile.am
             ;; generates 'lib/vlc/plugins/plugins.dat', a plugin cache, using
             ;; 'vlc-cache-gen'.  This file includes the mtime of the plugins
             ;; it references.  Thus, we first reset the timestamps of all
             ;; these files, and then regenerate the cache such that the
             ;; mtimes it includes are always zero instead of being dependent
             ;; on the build time.
             (let* ((out       (assoc-ref outputs "out"))
                    (pkglibdir (string-append out "/lib/vlc"))
                    (plugindir (string-append pkglibdir "/plugins"))
                    (cachegen  (string-append pkglibdir "/vlc-cache-gen")))
               ;; TODO: Factorize 'reset-timestamps'.
               (for-each (lambda (file)
                           (let ((s (lstat file)))
                             (unless (eq? (stat:type s) 'symlink)
                               (utime file 0 0 0 0))))
                         (find-files plugindir))
               (zero? (system* cachegen plugindir))))))))
    (home-page "https://www.videolan.org/")
    (synopsis "Audio and video framework")
    (description "VLC is a cross-platform multimedia player and framework
that plays most multimedia files as well as DVD, Audio CD, VCD, and various
treaming protocols.")
    (license license:gpl2+)))

(define-public mplayer
  (package
    (name "mplayer")
    (version "1.3.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.mplayerhq.hu/MPlayer/releases/MPlayer-"
                   version ".tar.xz"))
             (sha256
              (base32
               "0hwqn04bdknb2ic88xd75smffxx63scvz0zvwvjb56nqj9n89l1s"))))
    (build-system gnu-build-system)
    ;; FIXME: Add additional inputs once available.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cdparanoia" ,cdparanoia)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
;;        ("giflib" ,giflib) ; uses QuantizeBuffer, requires version >= 5
       ("lame" ,lame)
       ("libass" ,libass)
       ("libdvdcss" ,libdvdcss)
       ("libdvdnav" ,libdvdnav)
       ("libjpeg" ,libjpeg)
       ("libmpeg2" ,libmpeg2)
       ("libmpg123" ,mpg123)                      ; audio codec for MP3
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libx264" ,libx264)
       ("libxinerama" ,libxinerama)
       ("libxv" ,libxv)
       ("libxxf86dga" ,libxxf86dga)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("sdl" ,sdl)
       ("speex" ,speex)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
        (replace 'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (libx11 (assoc-ref inputs "libx11")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (zero? (system*
                      "./configure"
                      (string-append "--extra-cflags=-I"
                                     libx11 "/include") ; to detect libx11
                      "--disable-ffmpeg_a" ; disables bundled ffmpeg
                      (string-append "--prefix=" out)
                      ;; Enable runtime cpu detection where supported,
                      ;; and choose a suitable target.
                      ,@(match (or (%current-target-system)
                                   (%current-system))
                          ("x86_64-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=x86_64-linux"))
                          ("i686-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=i686-linux"))
                          ("mips64el-linux"
                           '("--target=mips3-linux"))
                          (_ (list (string-append
                                    "--target="
                                    (or (%current-target-system)
                                        (nix-system->gnu-triplet
                                         (%current-system)))))))
                      "--disable-iwmmxt"))))))))
    (home-page "https://www.mplayerhq.hu/design7/news.html")
    (synopsis "Audio and video player")
    (description "MPlayer is a movie player.  It plays most MPEG/VOB, AVI,
Ogg/OGM, VIVO, ASF/WMA/WMV, QT/MOV/MP4, RealMedia, Matroska, NUT,
NuppelVideo, FLI, YUV4MPEG, FILM, RoQ, PVA files.  One can watch VideoCD,
SVCD, DVD, 3ivx, DivX 3/4/5, WMV and H.264 movies.")
    (license license:gpl2)))

(define-public mpv
  (package
    (name "mpv")
    (version "0.25.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mpv-player/mpv/archive/v" version
                    ".tar.gz"))
              (sha256
               (base32
                "1khb7c4fdj1aak46lwyb3lq14w5jpxzws0zp6bdc87ljsvx3yhh7"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system waf-build-system)
    (native-inputs
     `(("perl" ,perl) ; for zsh completion file
       ("pkg-config" ,pkg-config)
       ("python-docutils" ,python-docutils)))
    ;; Missing features: libguess, Wayland, V4L2
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("enca" ,enca)
       ("ffmpeg" ,ffmpeg)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("lcms" ,lcms)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libbs2b" ,libbs2b)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libdvdread" ,libdvdread)
       ("libdvdnav" ,libdvdnav)
       ("libjpeg" ,libjpeg)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxv" ,libxv)
       ;; XXX: lua > 5.2 is not currently supported; see
       ;; waftools/checks/custom.py
       ("lua" ,lua-5.2)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("pulseaudio" ,pulseaudio)
       ("rsound" ,rsound)
       ("waf" ,python-waf)
       ("youtube-dl" ,youtube-dl)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'setup-waf
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-file (assoc-ref inputs "waf") "waf")
            (setenv "CC" "gcc"))))
       #:configure-flags (list "--enable-libmpv-shared"
                               "--enable-cdda"
                               "--enable-dvdread"
                               "--enable-dvdnav"
                               "--enable-zsh-comp"
                               "--disable-build-date")
       ;; No check function defined.
       #:tests? #f))
    (home-page "https://mpv.io/")
    (synopsis "Audio and video player")
    (description "mpv is a general-purpose audio and video player.  It is a
fork of mplayer2 and MPlayer.  It shares some features with the former
projects while introducing many more.")
    (license license:gpl2+)))

(define-public gnome-mpv
  (package
    (name "gnome-mpv")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gnome-mpv/gnome-mpv/releases"
                           "/download/v" version "/gnome-mpv-" version
                           ".tar.xz"))
       (sha256
        (base32
         "1hn3mpsxbrwf2m0nz4vzji4i6i896y8kqjb9kijqpk04cnrs3fgz"))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libepoxy" ,libepoxy)
       ("mpv" ,mpv)))
    (build-system glib-or-gtk-build-system)
    (home-page "https://github.com/gnome-mpv/gnome-mpv")
    (synopsis "GTK+ frontend for the mpv media player")
    (description "GNOME MPV is a simple GTK+ frontend for the mpv media player.
GNOME MPV interacts with mpv via the client API exported by libmpv, allowing
access to mpv's powerful playback capabilities.")
    (license license:gpl3+)))

(define-public libvpx
  (package
    (name "libvpx")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://storage.googleapis.com/"
                                  "downloads.webmproject.org/releases/webm/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "06d8hqjkfs6wl45qf4pwh1kpbvkx6cwywd5y8d4lgagvjwm0qb0w"))
              (patches (search-patches "libvpx-CVE-2016-2818.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CONFIG_SHELL" (which "bash"))
             (let ((out (assoc-ref outputs "out")))
               (setenv "LDFLAGS"
                       (string-append "-Wl,-rpath=" out "/lib"))
               (zero? (system* "./configure"
                               "--enable-shared"
                               "--as=yasm"
                               ;; Limit size to avoid CVE-2015-1258
                               "--size-limit=16384x16384"
                               (string-append "--prefix=" out)))))))
       #:tests? #f)) ; no check target
    (native-inputs
     `(("perl" ,perl)
       ("yasm" ,yasm)))
    (synopsis "VP8/VP9 video codec")
    (description "libvpx is a codec for the VP8/VP9 video compression format.")
    (license license:bsd-3)
    (home-page "http://www.webmproject.org/")))

(define-public youtube-dl
  (package
    (name "youtube-dl")
    (version "2017.05.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://yt-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1q3b9xkbk1lmy1wxm1jcnmaj70sdksqbc8zsqxz6b6z4nmv8qc25"))))
    (build-system python-build-system)
    (arguments
     ;; The problem here is that the directory for the man page and completion
     ;; files is relative, and for some reason, setup.py uses the
     ;; auto-detected sys.prefix instead of the user-defined "--prefix=FOO".
     ;; So, we need pass the prefix directly.  In addition, make sure the Bash
     ;; completion file is called 'youtube-dl' rather than
     ;; 'youtube-dl.bash-completion'.
     `(#:tests? #f ; Many tests fail. The test suite can be run with pytest.
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'fix-the-data-directories
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        (mkdir "bash-completion")
                        (rename-file "youtube-dl.bash-completion"
                                     "bash-completion/youtube-dl")
                        (substitute* "setup.py"
                          (("youtube-dl\\.bash-completion")
                           "bash-completion/youtube-dl")
                          (("'etc/")
                           (string-append "'" prefix "/etc/"))
                          (("'share/")
                           (string-append "'" prefix "/share/")))))))))
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "Youtube-dl is a small command-line program to download videos from
YouTube.com and a few more sites.")
    (home-page "https://yt-dl.org")
    (license license:public-domain)))

(define-public youtube-dl-gui
  (package
    (name "youtube-dl-gui")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Youtube-DLG" version))
       (sha256
        (base32
         "0napxwzgls5ik1bxbp99vly32l23xpc4ng5kr24hfhf21ypjyadb"))))
    (build-system python-build-system)
    (arguments
     ;; In Guix, wxpython has not yet been packaged for Python 3.
     `(#:python ,python-2
       ;; This package has no tests.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The youtube-dl-gui program lets you configure options.  Some of
             ;; them are problematic, so we change their defaults.
             (substitute* "youtube_dl_gui/optionsmanager.py"
               ;; When this is true, the builder process will try (and fail) to
               ;; write logs to the builder user's home directory.
               (("'enable_log': True") "'enable_log': False")
               ;; This determines which youtube-dl program youtube-dl-gui will
               ;; run.  If we don't set this, then youtube-dl-gui might download
               ;; an arbitrary copy from the Internet into the user's home
               ;; directory and run it, so let's make sure youtube-dl-gui uses
               ;; the youtube-dl from the inputs by default.
               (("'youtubedl_path': self.config_path")
                (string-append "'youtubedl_path': '"
                               (assoc-ref inputs "youtube-dl")
                               "/bin'"))
               ;; When this is True, when youtube-dl-gui is finished downloading
               ;; a file, it will try (and possibly fail) to open the directory
               ;; containing the downloaded file.  This can fail because it
               ;; assumes that xdg-open is in PATH.  Unfortunately, simply
               ;; adding xdg-utils to the propagated inputs is not enough to
               ;; make this work, so for now we set the default to False.
               (("'open_dl_dir': True") "'open_dl_dir': False"))
             ;; The youtube-dl program from the inputs is actually a wrapper
             ;; script written in bash, so attempting to invoke it as a python
             ;; script will fail.
             (substitute* "youtube_dl_gui/downloaders.py"
               (("cmd = \\['python', self\\.youtubedl_path\\]")
                "cmd = [self.youtubedl_path]"))
             ;; Use relative paths for installing data files so youtube-dl-gui
             ;; installs the files relative to its prefix in the store, rather
             ;; than relative to /.  Also, instead of installing data files into
             ;; $prefix/usr/share, install them into $prefix/share for
             ;; consistency (see: (standards) Directory Variables).
             (substitute* "setup.py"
               (("= '/usr/share") "= 'share"))
             ;; Update get_locale_file() so it finds the installed localization
             ;; files.
             (substitute* "youtube_dl_gui/utils.py"
               (("os\\.path\\.join\\('/usr', 'share'")
                (string-append "os.path.join('"
                               (assoc-ref %outputs "out")
                               "', 'share'"))))))))
    (inputs
     `(("python2-wxpython" ,python2-wxpython)
       ("youtube-dl" ,youtube-dl)))
    (home-page "https://github.com/MrS0m30n3/youtube-dl-gui")
    (synopsis
     "GUI (Graphical User Interface) for @command{youtube-dl}")
    (description
     "Youtube-dlG is a GUI (Graphical User Interface) for
@command{youtube-dl}.  You can use it to download videos from YouTube and any
other site that youtube-dl supports.")
    (license license:unlicense)))

(define-public you-get
  (package
    (name "you-get")
    (version "0.4.715")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/soimort/you-get/releases/download/v"
                    version "/you-get-" version ".tar.gz"))
              (sha256
               (base32
                "043122hfh56fbbszp1kwd1f65asgyn60j1ijday93hf2dkhvbrnh"))))
    (build-system python-build-system)
    (arguments
     ;; no tests
     '(#:tests? #f))
    (inputs
     `(("ffmpeg" ,ffmpeg)))
    (synopsis "Download videos, audios, or images from Web sites")
    (description
     "You-Get is a command-line utility to download media contents (videos,
audio, images) from the Web.  It can use either mpv or vlc for playback.")
    (home-page "https://you-get.org/")
    (license license:expat)))

(define-public libbluray
  (package
    (name "libbluray")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1k3lag4lxi2jjd3zh4wcb5l3hadzm54j5kagh92yzfy76p9svqzp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-bdjava")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dlopen-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libaacs (assoc-ref inputs "libaacs"))
                   (libbdplus (assoc-ref inputs "libbdplus")))
               (substitute* "src/libbluray/disc/aacs.c"
                 (("\"libaacs\"")
                  (string-append "\"" libaacs "/lib/libaacs\"")))
               (substitute* "src/libbluray/disc/bdplus.c"
                 (("\"libbdplus\"")
                  (string-append "\"" libbdplus "/lib/libbdplus\"")))
               #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libaacs" ,libaacs)
       ("libbdplus" ,libbdplus)
       ("libxml2" ,libxml2)))
    (home-page "https://www.videolan.org/developers/libbluray.html")
    (synopsis "Blu-Ray Disc playback library")
    (description
     "libbluray is a library designed for Blu-Ray Disc playback for media
players, like VLC or MPlayer.")
    (license license:lgpl2.1+)))

(define-public libdvdread
  (package
    (name "libdvdread")
    (version "5.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0ayqiq0psq18rcp6f5pz82sxsq66v0kwv0y55dbrcg68plnxy71j"))))
    (build-system gnu-build-system)
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for reading video DVDs")
    (description
     "Libdvdread provides a simple foundation for reading DVD video
disks.  It provides the functionality that is required to access many
DVDs.  It parses IFO files, reads NAV-blocks, and performs CSS
authentication and descrambling (if an external libdvdcss library is
installed).")
    (license license:gpl2+)))

(define-public libdvdnav
  (package
    (name "libdvdnav")
    (version "5.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0v8byv5z598k06rqzdmj7739vc86xq3zf79zfr698dib7lz055sh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdvdread" ,libdvdread)))
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for video DVD navigation features")
    (description
     "Libdvdnav is a library for developers of multimedia
applications.  It allows easy use of sophisticated DVD navigation features
such as DVD menus, multiangle playback and even interactive DVD games.  All
this functionality is provided through a simple API which provides the DVD
playback as a single logical stream of blocks, intermitted by special
dvdnav events to report certain conditions.  The main usage of libdvdnav is
a loop regularly calling a function to get the next block, surrounded by
additional calls to tell the library of user interaction.  The whole
DVD virtual machine and internal playback states are completely
encapsulated.")
    (license license:gpl2+)))

(define-public libdvdnav-4
  (package
    (inherit libdvdnav)
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://download.videolan.org/videolan/libdvdnav/libdvdnav-"
                version ".tar.xz"))
              (sha256
               (base32
                "0wi3gy408c8xj0ism0hckv5jbfh3lg4pmgxv87gbch9jrhp2gjkz"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:phases
       (alist-cons-after
        'unpack 'autoreconf
        (lambda _
          (zero? (system* "autoreconf" "-vif")))
        %standard-phases)))))

(define-public libdvdcss
  (package
    (name "libdvdcss")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/pub/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0nl45ifc4xcb196snv9d6hinfw614cqpzcqp92dg43c0hickg290"))))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libdvdcss.html")
    (synopsis "Library for accessing DVDs as block devices")
    (description
     "libdvdcss is a simple library designed for accessing DVDs like a block
device without having to bother about the decryption.")
    (license license:gpl2+)))

(define-public srt2vtt
  (package
    (name "srt2vtt")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dthompson.us/releases/srt2vtt/srt2vtt-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16b377znjm6qlga5yb8aj7b7bcisa1ghcnj2lrb1d30lvxp4liif"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.0)))
    (synopsis "SubRip to WebVTT subtitle converter")
    (description "srt2vtt converts SubRip formatted subtitles to WebVTT format
for use with HTML5 video.")
    (home-page "http://dthompson.us/pages/software/srt2vtt")
    (license license:gpl3+)))

(define-public avidemux
  (package
    (name "avidemux")
    (version "2.6.12")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/" name "/" name "/" version "/"
                   name "_" version ".tar.gz"))
             (sha256
              (base32
               "0nz52yih8sff53inndkh2dba759xjzsh4b8xjww419lcpk0qp6kn"))
             (patches (search-patches "avidemux-install-to-lib.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; FIXME: Once packaged, add libraries not found during the build.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glu" ,glu)
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxv" ,libxv)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("qt" ,qt) ; FIXME: reenable modular qt after update - requires building
       ;("qtbase" ,qtbase) with -std=gnu++11.
       ;("qttools" ,qttools)
       ("sdl" ,sdl)
       ("sqlite" ,sqlite)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       ;; Make sure files inside the included ffmpeg tarball are
       ;; patch-shebanged.
       (modify-phases %standard-phases
       (add-before 'patch-source-shebangs 'unpack-ffmpeg
         (lambda _
           (with-directory-excursion "avidemux_core/ffmpeg_package"
             (system* "tar" "xf" "ffmpeg-2.7.6.tar.bz2")
             (delete-file "ffmpeg-2.7.6.tar.bz2"))))
       (add-after 'patch-source-shebangs 'repack-ffmpeg
         (lambda _
           (with-directory-excursion "avidemux_core/ffmpeg_package"
             (substitute* "ffmpeg-2.7.6/configure"
               (("#! /bin/sh") (string-append "#!" (which "sh"))))
             (system* "tar" "cjf" "ffmpeg-2.7.6.tar.bz2" "ffmpeg-2.7.6"
                      ;; avoid non-determinism in the archive
                      "--sort=name" "--mtime=@0"
                      "--owner=root:0" "--group=root:0")
             (delete-file-recursively "ffmpeg-2.7.6"))))
       (replace 'configure
         (lambda _
           ;; Copy-paste settings from the cmake build system.
           (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
           (setenv "CMAKE_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))))
       (replace 'build
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let*
             ((out (assoc-ref outputs "out"))
              (lib (string-append out "/lib"))
              (top (getcwd))
              (sdl (assoc-ref inputs "sdl"))
              (build_component
                (lambda* (component srcdir #:optional (args '()))
                  (let ((builddir (string-append "build_" component)))
                    (mkdir builddir)
                    (with-directory-excursion builddir
                      (zero?
                        (and
                          (apply system* "cmake"
                                 "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                                 (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                 (string-append "-DCMAKE_INSTALL_RPATH=" lib)
                                 (string-append "-DCMAKE_SHARED_LINKER_FLAGS="
                                                "\"-Wl,-rpath=" lib "\"")
                                 (string-append "-DAVIDEMUX_SOURCE_DIR=" top)
                                 (string-append "-DSDL_INCLUDE_DIR="
                                                sdl "/include/SDL")
                                 (string-append "../" srcdir)
                                 "-DENABLE_QT5=True"
                                 args)
                         (system* "make" "-j"
                                 (number->string (parallel-job-count)))
                         (system* "make" "install"))))))))
             (mkdir out)
             (and (build_component "core" "avidemux_core")
                  (build_component "cli" "avidemux/cli")
                  (build_component "qt4" "avidemux/qt4")
                  (build_component "plugins_common" "avidemux_plugins"
                                  '("-DPLUGIN_UI=COMMON"))
                  (build_component "plugins_cli" "avidemux_plugins"
                                  '("-DPLUGIN_UI=CLI"))
                  (build_component "plugins_qt4" "avidemux_plugins"
                                  '("-DPLUGIN_UI=QT4"))
                  (build_component "plugins_settings" "avidemux_plugins"
                                  '("-DPLUGIN_UI=SETTINGS")))
             ;; Remove .exe and .dll file.
             (delete-file-recursively
               (string-append out "/share/ADM6_addons")))))
       (delete 'install))))
    (home-page "http://fixounet.free.fr/avidemux/")
    (synopsis "Video editor")
    (description "Avidemux is a video editor designed for simple cutting,
filtering and encoding tasks.  It supports many file types, including AVI,
DVD compatible MPEG files, MP4 and ASF, using a variety of codecs.  Tasks
can be automated using projects, job queue and powerful scripting
capabilities.")
    ;; Software with various licenses is included, see License.txt.
    (license license:gpl2+)))

(define-public vapoursynth
  (package
    (name "vapoursynth")
    (version "37")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/vapoursynth/vapoursynth/archive/R"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g3hc079jw4mz1cmkv2y28pdb556wqc8ql7iravgh1rg8j3f1zi5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("cython" ,python-cython)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("yasm" ,yasm)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("libass" ,libass)
       ("tesseract-ocr" ,tesseract-ocr)
       ("zimg" ,zimg)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'autogen
          (lambda _
            (zero? (system* "sh" "autogen.sh")))))))
    (home-page "http://www.vapoursynth.com/")
    (synopsis "Video processing framework")
    (description "VapourSynth is a C++ library and Python module for video
manipulation.  It aims to be a modern rewrite of Avisynth, supporting
multithreading, generalized colorspaces, per frame properties, and videos with
format changes.")
    ;; src/core/cpufeatures only allows x86, ARM or PPC
    (supported-systems (delete "mips64el-linux" %supported-systems))
    ;; As seen from the source files.
    (license license:lgpl2.1+)))

(define-public xvid
  (package
    (name "xvid")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.xvid.org/downloads/xvidcore-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1xwbmp9wqshc0ckm970zdpi0yvgqxlqg0s8bkz98mnr8p2067bsz"))))
    (build-system gnu-build-system)
    (native-inputs `(("yasm" ,yasm)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda _
            (chdir "build/generic")
            (substitute* "configure"
              (("#! /bin/sh") (string-append "#!" (which "sh")))))))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.xvid.com/")
    (synopsis "MPEG-4 Part 2 Advanced Simple Profile video codec")
    (description "Xvid is an MPEG-4 Part 2 Advanced Simple Profile (ASP) video
codec library.  It uses ASP features such as b-frames, global and quarter
pixel motion compensation, lumi masking, trellis quantization, and H.263, MPEG
and custom quantization matrices.")
    (license license:gpl2+)))

(define-public livestreamer
  (package
    (name "livestreamer")
    (version "1.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/chrippa/livestreamer/archive/v"
                    version ".tar.gz"))
              (file-name (string-append "livestreamer-" version ".tar.gz"))
              (sha256
               (base32
                "1fp3d3z2grb1ls97smjkraazpxnvajda2d1g1378s6gzmda2jvjd"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests rely on external web servers
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (synopsis "Internet video stream viewer")
    (description "Livestreamer is a command-line utility that extracts streams
from various services and pipes them into a video playing application.")
    (home-page "http://livestreamer.io/")
    (license license:bsd-2)))

(define-public mlt
  (package
    (name "mlt")
    (version "6.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mltframework/mlt/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10m3ry0b2pvqx3bk34qh5dq337nn8pkc2gzfyhsj4nv9abskln47"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags '("CC=gcc")
       #:configure-flags
       (list "--enable-gpl3"
             "--enable-gpl")
       #:phases
       (modify-phases %standard-phases
         (add-after
          'configure 'override-LDFLAGS
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "config.mak"
              (("LDFLAGS\\+=")
               (string-append "LDFLAGS+=-Wl,-rpath="
                              (assoc-ref outputs "out")
                              "/lib ")))
            #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)
       ("libxml2" ,libxml2)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("libsamplerate" ,libsamplerate)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl)
       ("sox" ,sox)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.mltframework.org/")
    (synopsis "Author, manage, and run multitrack audio/video compositions")
    (description
     "MLT is a multimedia framework, designed and developed for television
broadcasting.  It provides a toolkit for broadcasters, video editors, media
players, transcoders, web streamers and many more types of applications.  The
functionality of the system is provided via an assortment of ready to use
tools, XML authoring components, and an extensible plug-in based API.")
    (license license:lgpl2.1+)))

(define-public v4l-utils
  (package
    (name "v4l-utils")
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://linuxtv.org/downloads/v4l-utils"
                                  "/v4l-utils-" version ".tar.bz2"))
              (sha256
               (base32
                "0vpl3jl0x441y7b5cn7zhdsyi954hp9h2p30jhnr1zkx1rpxsiss"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-udevdir="
                            (assoc-ref %outputs "out")
                            "/lib/udev")
             "CXXFLAGS=-std=gnu++11")))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg)
       ("libx11" ,libx11)
       ("qtbase" ,qtbase)
       ("eudev" ,eudev)))
    (synopsis "Realtime video capture utilities for Linux")
    (description "The v4l-utils provide a series of libraries and utilities to
be used for realtime video capture via Linux-specific APIs.")
    (home-page "https://linuxtv.org/wiki/index.php/V4l-utils")
    ;; libv4l2 is LGPL2.1+, while utilities are GPL2 only.
    (license (list license:lgpl2.1+ license:gpl2))))

(define-public obs
  (package
    (name "obs")
    (version "18.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jp9000/obs-studio"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02pbiyvf5x0zh448h5rpmyn33qnsqk694xxlyns83mdi74savyqw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("jack" ,jack-1)
       ("jansson" ,jansson)
       ("libx264" ,libx264)
       ("libxcomposite" ,libxcomposite)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("speex" ,speex)
       ("v4l-utils" ,v4l-utils)
       ("zlib" ,zlib)))
    (synopsis "Live streaming software")
    (description "Open Broadcaster Software provides a graphical interface for
video recording and live streaming.  OBS supports capturing audio and video
from many input sources such as webcams, X11 (for screencasting), PulseAudio,
and JACK.")
    (home-page "https://obsproject.com")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:gpl2+)))

(define-public libvdpau
  (package
    (name "libvdpau")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://secure.freedesktop.org/~aplattner/vdpau/"
                            name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0dnpb0yh7v6rvckx82kxg045rd9rbsw25wjv7ad5n8h94s9h2yl5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("dri2proto" ,dri2proto)
       ("libx11" ,libx11 "out")
       ("libxext" ,libxext)))
    (home-page "https://wiki.freedesktop.org/www/Software/VDPAU/")
    (synopsis "Video Decode and Presentation API")
    (description "VDPAU is the Video Decode and Presentation API for UNIX.  It
provides an interface to video decode acceleration and presentation hardware
present in modern GPUs.")
    (license (license:x11-style "file://COPYING"))))

(define-public vdpauinfo
  (package
    (name "vdpauinfo")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://secure.freedesktop.org/~aplattner/vdpau/"
                            name "-" version ".tar.gz"))
        (sha256
         (base32
          "1i2b0k9h8r0lnxlrkgqzmrjakgaw3f1ygqqwzx8w6676g85rcm20"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libx11" ,libx11)))
    (propagated-inputs
     `(("libvdpau" ,libvdpau)))
    (home-page "https://wiki.freedesktop.org/www/Software/VDPAU/")
    (synopsis "Tool to query the capabilities of a VDPAU implementation")
    (description "Vdpauinfo is a tool to query the capabilities of a VDPAU
implementation.")
    (license (license:x11-style "file://COPYING"))))

(define-public recordmydesktop
  (package
    (name "recordmydesktop")
    (version "0.3.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  version "/recordmydesktop-" version ".tar.gz"))
              (sha256
               (base32
                "133kkl5j0r877d41bzj7kj0vf3xm8x80yyx2n8nqxrva304f58ik"))))
    (build-system gnu-build-system)
    (inputs `(("popt" ,popt)
              ("zlib" ,zlib)
              ("libx11" ,libx11)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("libxfixes" ,libxfixes)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("alsa-lib" ,alsa-lib)
              ("libvorbis" ,libvorbis)
              ("libtheora" ,libtheora)))
    (home-page "http://recordmydesktop.sourceforge.net/")
    (synopsis "Desktop session video recorder")
    (description
     "recordMyDesktop is a command-line tool that captures the activity in
your graphical desktop and encodes it as a video.  This is a useful tool for
making @dfn{screencasts}.")
    (license license:gpl2+)))

(define-public libsmpeg
  (package
    (name "libsmpeg")
    (version "0.4.5")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "svn://svn.icculus.org/smpeg/trunk/")
                    (revision 401))) ; last revision before smpeg2 (for SDL 2.0)
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "18yfkr70lr1x1hc8snn2ldnbzdcc7b64xmkqrfk8w59gpg7sl1xn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autogen.sh
                    (lambda _
                      (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("sdl" ,sdl2)))
    (home-page "http://icculus.org/smpeg/")
    (synopsis "SDL MPEG decoding library")
    (description
     "SMPEG (SDL MPEG Player Library) is a free MPEG1 video player library
with sound support.  Video playback is based on the ubiquitous Berkeley MPEG
player, mpeg_play v2.2.  Audio is played through a slightly modified mpegsound
library, part of splay v0.8.2.  SMPEG supports MPEG audio (MP3), MPEG-1 video,
and MPEG system streams.")
    (license (list license:expat
                   license:lgpl2.1
                   license:lgpl2.1+
                   license:gpl2))))

(define-public libbdplus
  (package
    (name "libbdplus")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.videolan.org/pub/videolan/libbdplus/"
                           version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "02n87lysqn4kg2qk7d1ffrp96c44zkdlxdj0n16hbgrlrpiwlcd6"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libbdplus.html")
    (synopsis "Library for decrypting certain Blu-Ray discs")
    (description "libbdplus is a library which implements the BD+ System
specifications.")
    (license license:lgpl2.1+)))

(define-public libaacs
  (package
    (name "libaacs")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.videolan.org/pub/videolan/libaacs/"
                           version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1s5v075hnbs57995r6lljm79wgrip3gnyf55a0y7bja75jh49hwm"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libaacs.html")
    (synopsis "Library for decrypting certain Blu-Ray discs")
    (description "libaacs is a library which implements the Advanced Access
Content System specification.")
    (license license:lgpl2.1+)))

(define-public mps-youtube
  (package
    (name "mps-youtube")
    (version "0.2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mps-youtube/mps-youtube/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s7h35yx6f0szf8mm8612ic913w3v05m2kwphjfcxnpq0ammhyci"))))
    (build-system python-build-system)
    (arguments
     ;; Tests need to be disabled until #556 upstream is fixed. It reads as if the
     ;; test suite results differ depending on the country and also introduce
     ;; non-determinism in the tests.
     ;; https://github.com/mps-youtube/mps-youtube/issues/556
     `(#:tests? #f))
    (propagated-inputs
     `(("python-pafy" ,python-pafy)
       ("python-pygobject" ,python-pygobject))) ; For mpris2 support
    (home-page "https://github.com/mps-youtube/mps-youtube")
    (synopsis "Terminal based YouTube player and downloader")
    (description
     "@code{mps-youtube} is based on mps, a terminal based program to
search, stream and download music.  This implementation uses YouTube as
a source of content and can play and download video as well as audio.
It can use either mpv or mplayer for playback, and for conversion of
formats ffmpeg or libav is used.  Users should install one of the
supported players in addition to this package.")
    (license license:gpl3+)))

(define-public handbrake
  (package
    (name "handbrake")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://handbrake.fr/rotation.php?file="
                                  "HandBrake-" version ".tar.bz2"))
              (file-name (string-append "handbrake-" version ".tar.bz2"))
              (sha256
               (base32
                "1w720y3bplkz187wgvy4a4xm0vpppg45mlni55l6yi8v2bfk14pv"))
              (patches (search-patches "handbrake-pkg-config-path.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled libraries and source not necessary for
               ;; running under a GNU environment.
               '(begin
                  (for-each delete-file-recursively '("contrib" "macosx" "win"))
                  #t))))
    (build-system  glib-or-gtk-build-system)
    (native-inputs
     `(("automake" ,automake)           ;gui subpackage must be bootstrapped
       ("autoconf" ,autoconf)
       ("curl" ,curl)                   ;not actually used, but tested for
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))           ;for configuration
    (inputs
     `(("bzip2" ,bzip2)
       ("dbus-glib" ,dbus-glib)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("lame" ,lame)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libdvdnav" ,libdvdnav)
       ("libdvdread" ,libdvdread)
       ("libgudev" ,libgudev)
       ("libmpeg2" ,libmpeg2)
       ("libnotify" ,libnotify)
       ("libogg" ,libogg)
       ("libsamplerate" ,libsamplerate)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxml2" ,libxml2)
       ("libx264" ,libx264)
       ("x265" ,x265)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f             ;tests require Ruby and claim to be unsupported
       #:phases
       (modify-phases %standard-phases
         (add-before 'patch-source-shebangs 'bootstrap-gtk
           ;; Run bootstrap ahead of time so that shebangs get patched.
           (lambda _
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "NOCONFIGURE" "1")
             ;; Patch the Makefile so that it doesn't bootstrap again.
             (substitute* "gtk/module.rules"
               ((".*autogen\\.sh.*") ""))
             (zero? (system* "sh" "./gtk/autogen.sh"))))
         (add-before 'configure 'disable-contrib
           (lambda _
             (substitute* "make/include/main.defs"
               ;; Disable unconditional inclusion of some "contrib"
               ;; libraries (ffmpeg, libvpx, libdvdread, libdvdnav,
               ;; and libbluray), which would lead to fetching and
               ;; building of these libraries.  Use our own instead.
               (("MODULES \\+= contrib") "# MODULES += contrib"))
             #t))
         (add-before 'configure 'fix-x265-linking
           (lambda _
             (substitute* "test/module.defs"
               ;; Fix missing library during linking error
               (("TEST.GCC.l =") "TEST.GCC.l = x265"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             ;; 'configure' is not an autoconf-generated script, and
             ;; errors on unrecognized arguments,
             ;; e.g. --enable-fast-install
             (let ((out (assoc-ref outputs "out")))
               (zero? (apply system* "./configure"
                             (string-append "--prefix=" out)
                             (or configure-flags '()))))))
         (add-after 'configure 'chdir-build
           (lambda _ (chdir "./build") #t)))))
    (home-page "https://handbrake.fr")
    (synopsis "Video transcoder")
    (description
     "HandBrake is a tool for converting video from any format to a selection
of modern, widely supported codecs.")
    ;; Most under GPL version 2 or later, and portions under BSD 3 Clause
    (license (list license:gpl2+ license:bsd-3))))

(define-public openh264
  (package
    (name "openh264")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cisco/"
                                  name "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ix2fhk62i4q4kbnkl0gfk4x53vxqavsn0pck1pashr566zhglv5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("nasm" ,nasm)
       ("python" ,python)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  ;; no configure script
                  (delete 'configure))))
    (home-page "http://www.openh264.org/")
    (synopsis "H264 decoder library")
    (description
     "Openh264 is a library which can decode H264 video streams.")
    (license license:bsd-2)))

(define-public libmp4v2
  (package
    (name "libmp4v2")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       ;; XXX: The new location of upstream is uncertain and will become relevant the
       ;; moment when the googlecode archive shuts down. It is past the date it
       ;; should've been turned off. I tried to communicate with upstream, but this
       ;; wasn't very responsive and not very helpful. The short summary is, it is
       ;; chaos when it comes to the amount of forks and only time will tell where
       ;; the new upstream location is.
       (uri (string-append "https://storage.googleapis.com/google-"
                           "code-archive-downloads/v2/"
                           "code.google.com/mp4v2/mp4v2-" version ".tar.bz2"))
       (file-name (string-append name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0f438bimimsvxjbdp4vsr8hjw2nwggmhaxgcw07g2z361fkbj683"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static")) ; 3.7MiB .a file
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-dates
           (lambda _
             ;; Make the build reproducible.
             (substitute* "configure"
               (("PROJECT_build=\"`date`\"") "PROJECT_build=\"\"")
               (("ac_abs_top_builddir=$ac_pwd") "ac_abs_top_builddir=\"\""))
             #t))
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     `(("help2man" ,help2man)
       ("dejagnu" ,dejagnu)))
    (home-page "https://code.google.com/archive/p/mp4v2/")
    (synopsis "API to create and modify mp4 files")
    (description
     "The MP4v2 library provides an API to create and modify mp4 files as defined by
ISO-IEC:14496-1:2001 MPEG-4 Systems.  This file format is derived from Apple's QuickTime
file format that has been used as a multimedia file format in a variety of platforms and
applications.  It is a very powerful and extensible format that can accommodate
practically any type of media.")
    (license license:mpl1.1)))
