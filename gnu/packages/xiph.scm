;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages xiph)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp))

(define-public libogg
  (package
   (name "libogg")
   (version "1.3.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://downloads.xiph.org/releases/ogg/libogg-"
                                version ".tar.xz"))
            (sha256
             (base32
              "01b7050bghdvbxvw0gzv588fn4a27zh42ljpwzm4vrf8dziipnf4"))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags '("--disable-static")))
   (synopsis "Library for manipulating the ogg multimedia format")
   (description
    "The libogg library manipulates the ogg multimedia container
format, which encapsulates raw compressed data and allows the interleaving of
audio and video data.  In addition to encapsulation and interleaving of
multiple data streams, ogg provides packet framing, error detection, and
periodic timestamps for seeking.")
   (license (license:non-copyleft "file://COPYING"
                               "See COPYING in the distribution."))
   (home-page "https://xiph.org/ogg/")))

(define-public libvorbis
  (package
   (name "libvorbis")
   (version "1.3.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://downloads.xiph.org/releases/vorbis/"
                                "libvorbis-" version ".tar.xz"))
            (sha256
             (base32
              "0jwmf87x5sdis64rbv0l87mdpah1rbilkkxszipbzg128f9w8g5k"))))
   (build-system gnu-build-system)
   (propagated-inputs (list libogg))
   (arguments `(#:configure-flags '("LDFLAGS=-lm"
                                    "--disable-static")
                #:parallel-tests? #f))
   (synopsis "Library implementing the vorbis audio format")
   (description
    "The libvorbis library implements the ogg vorbis audio format,
a fully open, non-proprietary, patent-and-royalty-free, general-purpose
compressed audio format for mid to high quality (8kHz-48.0kHz, 16+ bit,
polyphonic) audio and music at fixed and variable bitrates from 16 to
128 kbps/channel.")
   ;; This package shows a sizable speed increase when tuned.
   (properties `((tunable? . #t)))
   (license (license:non-copyleft "file://COPYING"
                               "See COPYING in the distribution."))
   (home-page "https://xiph.org/vorbis/")))

(define-public libtheora
  (package
    (name "libtheora")
    (version "1.1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://downloads.xiph.org/releases/theora/"
                                 "libtheora-" version ".tar.xz"))
             (sha256
              (base32
               "0q8wark9ribij57dciym5vdikg2464p8q2mgqvfb78ksjh4s8vgk"))
             (patches (search-patches "libtheora-config-guess.patch"))))
    (build-system gnu-build-system)
    (arguments
     (append
      (if (and (target-riscv64?)
               (%current-target-system))
          (list #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'update-config
                      (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        (for-each (lambda (file)
                                    (install-file
                                     (search-input-file
                                      (or native-inputs inputs)
                                      (string-append "/bin/" file)) "."))
                                  '("config.guess" "config.sub"))))))
          '())
      (list #:configure-flags #~'("--disable-static"))))
    (native-inputs
     (if (and (target-riscv64?)
              (%current-target-system))
         (list config)
         '()))
    (inputs (list libvorbis))
    ;; The .pc files refer to libogg.
    (propagated-inputs (list libogg))
    (synopsis "Library implementing the Theora video format")
    (description
     "The libtheora library implements the ogg theora video format,
a fully open, non-proprietary, patent-and-royalty-free, general-purpose
compressed video format.")
    (license license:bsd-3)
    (home-page "https://xiph.org/theora/")))

(define-public speex
  (package
    (name "speex")
    (version "1.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://downloads.xiph.org/releases/speex/speex-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1spy51kaxfhpj8171gn1s69wz82nzkz2k0x7k0nhldwangrd8i2b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libogg speexdsp))
    (home-page "https://gnu.org/software/speex")
    (synopsis "Library for patent-free audio compression format")
    (description
     "GNU Speex is a patent-free audio compression codec specially designed
for speech.  It is well-adapted to internet applications, such as VoIP.  It
features compression of different bands in the same bitstream, intensity
stereo encoding, and voice activity detection.")
    ;; 'src/getopt.c' is under LGPLv2+
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public speexdsp
  (package
    (name "speexdsp")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.xiph.org/releases/speex/"
                                  "speexdsp-" version ".tar.gz"))
              (sha256
               (base32
                "0gadnnpg9994cindpnw35j45dnr4bflkig1aqxlrafd6wi1p6xwc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           ,@(if (string=? "aarch64-linux"
                                           (%current-system))
                               '("--enable-neon=no") ; neon defaults to armv7-a
                               '()))))
    (home-page "https://speex.org/")
    (synopsis "Speex processing library")
    (description
     "SpeexDSP is a @dfn{DSP} (Digital Signal Processing) library based on
work from the @code{speex} codec.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public ao
  (package
    (name "ao")
    ;; We need a few commits on top of 1.2.2 to fix CVE-2017-11548.
    (version "1.2.2-5-g20dc8ed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.xiph.org/xiph/libao")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d1b3g2a7jd43c32242yq6nfysqsmp7rjslhvbrmpgk119l5fnbj"))))
    (build-system gnu-build-system)
    ;; FIXME: Add further backends, see the summary printed after configure.
    ;; XXX: Should back-ends be pushed to different outputs?  For instance,
    ;; "out" would include only the ALSA back-end, while "pulse" would
    ;; contain 'lib/ao/plugins-4/libpulse.*'.
    (inputs
     (list alsa-lib pulseaudio))
    (native-inputs
     (list pkg-config autoconf automake libtool))
    (synopsis "Cross platform audio library")
    (description
     "Libao is a cross-platform audio library that allows programs to
output audio using a simple API on a wide variety of platforms.
It currently supports:
@enumerate
@item Null output (handy for testing without a sound device),
@item WAV files,
@item AU files,
@item RAW files,
@item OSS (Open Sound System, used on Linux and FreeBSD),
@item ALSA (Advanced Linux Sound Architecture),
@item aRts (Analog RealTime Synth, used by KDE),
@item PulseAudio (next generation GNOME sound server),
@item esd (EsounD or Enlightened Sound Daemon),
@item Mac OS X,
@item Windows (98 and later),
@item AIX,
@item Sun/NetBSD/OpenBSD,
@item IRIX,
@item NAS (Network Audio Server),
@item RoarAudio (Modern, multi-OS, networked Sound System),
@item OpenBSD's sndio.
@end enumerate
")
    (license license:gpl2+)
    (properties '((cpe-name . "libao")))
    (home-page "https://www.xiph.org/ao/")))

(define-public flac
  (package
   (name "flac")
   (version "1.4.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://downloads.xiph.org/releases/flac/flac-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0w2v40kmvl741vmycv8h5s10n7arbs12n2b1p10z8j13saffcn3c"))))
   (build-system gnu-build-system)
   (arguments
    `(#:parallel-tests? #f
      ;; Unfortunately we need to make some changes to work around an
      ;; assembly generation errors when building for armhf-linux.
      #:phases
      ,@(if (target-arm32?)
            `((modify-phases %standard-phases
                (add-before 'configure 'patch-configure
                  (lambda _
                    (substitute* "configure"
                      (("-O3") "-O2"))))))
            '(%standard-phases))))
   ;; FIXME: configure also looks for xmms, input could be added once it exists
   (propagated-inputs (list libogg)) ; required by flac.pc
   (synopsis "Free lossless audio codec")
   (description
"FLAC stands for Free Lossless Audio Codec, an audio format that is lossless,
meaning that audio is compressed in FLAC without any loss in quality.")
   (license (license:non-copyleft "file://COPYING"
                               "See COPYING in the distribution.")) ; and LGPL and GPL
   (home-page "https://xiph.org/flac/")))

(define-public libkate
  (package
   (name "libkate")
   (version "0.4.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://downloads.xiph.org/releases/kate/"
                                "libkate-" version ".tar.gz"))
            (sha256
             (base32
              "0s3vr2nxfxlf1k75iqpp4l78yf4gil3f0v778kvlngbchvaq23n4"))))
   (build-system gnu-build-system)
   (native-inputs (list doxygen bison pkg-config))
   ;; FIXME: Add optional input liboggz
   (inputs (list libogg libpng python-wrapper zlib))
   (synopsis "Karaoke and text codec for embedding in ogg")
   (description
    "Kate is an overlay codec, originally designed for karaoke and text,
that can be multiplixed in Ogg.  Text and images can be carried by a Kate
stream, and animated.  Most of the time, this would be multiplexed with
audio/video to carry subtitles, song lyrics (with or without karaoke data),
etc., but doesn't have to be.

Series of curves (splines, segments, etc.) may be attached to various
properties (text position, font size, etc.) to create animated overlays.
This allows scrolling or fading text to be defined.  This can even be used
to draw arbitrary shapes, so hand drawing can also be represented by a
Kate stream.")
   (license license:bsd-3)
   (home-page "https://wiki.xiph.org/OggKate")))

(define-public vorbis-tools
  (package
   (name "vorbis-tools")
   (version "1.4.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://downloads.xiph.org/releases/vorbis/"
                                "vorbis-tools-" version ".tar.gz"))
            (sha256
             (base32
              "1phd8vh66sw1df6crzsn8yr58ya3w3gpwzkrdfzwxgbpczf3vzm1"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:configure-flags
     ;; Relax gcc-14's strictness.
     #~(list "CFLAGS=-g -O2 -Wno-error=implicit-function-declaration")))
   (inputs (list ao
                 curl
                 flac
                 libkate
                 libogg
                 libvorbis
                 speex))
   (native-inputs (list pkg-config))
   (synopsis "Ogg vorbis tools")
   (description
    "Ogg vorbis is a non-proprietary, patent-and-royalty-free,
general-purpose compressed audio format.

The package vorbis-tools contains
ogg123,  an ogg vorbis command line audio player;
oggenc,  the ogg vorbis encoder;
oggdec,  a simple, portable command line decoder (to wav and raw);
ogginfo, to obtain information (tags, bitrate, length, etc.) about
         an ogg vorbis file.")
   (license license:gpl2)
   (home-page "https://xiph.org/vorbis/")))

(define-public opus
  (package
    (name "opus")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/opus/opus-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17gz8kxs4i7icsc1gj713gadiapyklynlwqlf0ai98dj4lg8xdb5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (synopsis "Versatile audio codec")
    (description
     "Opus is a totally open, royalty-free, highly versatile audio codec.  Opus
is unmatched for interactive speech and music transmission over the Internet,
but is also intended for storage and streaming applications.  It is
standardized by the Internet Engineering Task Force (IETF) as RFC 6716 which
incorporated technology from Skype's SILK codec and Xiph.Org's CELT codec.")
    ;; This package shows a sizable speed increase when tuned.
    (properties `((tunable? . #t)
                  (lint-hidden-cpe-vendors . ("discordjs"))
                  (release-monitoring-url
                   . "https://archive.mozilla.org/pub/opus/")))
    (license license:bsd-3)
    (home-page "https://www.opus-codec.org")))

(define-public opus-tools
  (package
    (name "opus-tools")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.xiph.org/releases/opus/opus-tools-"
                    version ".tar.gz"))
              (sha256
               (base32
                "11pzl27s4vcz4m18ch72nivbhww2zmzn56wspb7rll1y1nq6rrdl"))))
    (build-system gnu-build-system)
    (arguments
     ;; The package developers misuse pkg-config such that it doesn't work
     ;; when cross compiling.  Therefore we avoid it completly and set the
     ;; necessary flags ourselves.
     `(#:configure-flags (list (string-append "CFLAGS=-I"
                                              (assoc-ref %build-inputs "libogg")
                                              "/include -I"
                                              (assoc-ref %build-inputs "opus")
                                              "/include/opus"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libopusenc opusfile flac))
    (synopsis
     "Command line utilities to encode, inspect, and decode .opus files")
    (description "Opus is a royalty-free, highly versatile audio codec.
Opus-tools provide command line utilities for creating, inspecting and
decoding .opus files.")
    (license license:bsd-3)
    (home-page "https://www.opus-codec.org")))

(define-public opusfile
  (package
    (name "opusfile")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.xiph.org/releases/opus/opusfile-" version
                    ".tar.gz"))
              (sha256
               (base32
                "02smwc5ah8nb3a67mnkjzqmrzk43j356hgj2a97s9midq40qd38i"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-multistream
           ;; Opus include directory should be passed explicitly:
           ;; https://github.com/xiph/opusfile/issues/10 however,
           ;; opus_multistream.h still can't be found by the compiler.
           (lambda _
             (substitute* "include/opusfile.h"
               (("opus_multistream\\.h") "opus/opus_multistream.h")))))))
    ;; Required by opusfile.pc and opusurl.pc.
    (propagated-inputs
     (list libogg openssl opus))
    (native-inputs
     (list pkg-config))
    (synopsis "Versatile audio codec")
    (description
     "The opusfile library provides seeking, decode, and playback of Opus
streams in the Ogg container (.opus files) including over http(s) on posix and
windows systems.")
    (license license:bsd-3)
    (home-page "https://www.opus-codec.org")))

(define-public libopusenc
  (package
    (name "libopusenc")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/opus/"
                                  "libopusenc-" version ".tar.gz"))
              (sha256
               (base32
                "1ffb0vhlymlsq70pxsjj0ksz77yfm2x0a1x8q50kxmnkm1hxp642"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list opus))
    (synopsis "Library for encoding Opus audio files and streams")
    (description "The libopusenc libraries provide a high-level API for
encoding Opus files and streams.")
    (home-page "https://www.opus-codec.org/")
    (license license:bsd-3)))

(define-public icecast
  (package
    (name "icecast")
    (version "2.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.xiph.org/releases/icecast/icecast-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0i2d9rhav0x6js2qhjf5iy6j2a7f0d11ail0lfv40hb1kygrgda9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxslt
           libxml2
           openssl
           curl
           libogg
           libvorbis
           libtheora
           speex))
    (synopsis "Streaming media server")
    (description "Icecast is a streaming media server which currently supports
Ogg (Vorbis and Theora), Opus, WebM and MP3 audio streams.  It can be used to
create an Internet radio station or a privately running jukebox and many
things in between.")
    (home-page "https://icecast.org/")
    (license license:gpl2)))

(define-public libshout
  (package
    (name "libshout")
    (version "2.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.xiph.org/releases/libshout/"
                    "libshout-" version ".tar.gz"))
              (sha256
               (base32
                "0469yzc1csm25f5dbyb7ly7i1mzjz13pw8c8bmswkpfzxzqd9jrr"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list
                           #$(string-append
                              "CFLAGS=-g -O2"
                              " -Wno-error=implicit-function-declaration"))))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; shout.pc refers to all these.
     (list libtheora libvorbis speex))
    (home-page "https://icecast.org/")
    (synopsis "Audio streaming library for icecast encoders")
    (description
     "Libshout is a library for communicating with and sending data to an
icecast server.  It handles the socket connection, the timing of the data,
and prevents bad data from getting to the icecast server.")
    (license license:gpl2+)))

(define-public rnnoise
  ;; No upstream release
  (let ((commit "7f449bf8bd3b933891d12c30112268c4090e4d59")
        (revision "0"))
   (package
     (name "rnnoise")
     (version (git-version "0" revision commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://gitlab.xiph.org/xiph/rnnoise")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0y8jdvxd9namw4f9hcmcmm2q0f32mnhfyjap8906hl308cws3rkj"))))
     (build-system gnu-build-system)
     (native-inputs
      (list autoconf automake libtool))
     (home-page "https://gitlab.xiph.org/xiph/rnnoise")
     (synopsis "Real-time noise suppression")
     (description "RNNoise is a noise suppression library based on a recurrent
neural network.  The algorithm is described in Jean-Marc Valin's paper
@cite{A Hybrid DSP/Deep Learning Approach to Real-Time Full-Band Speech
Enhancement}.")
     (license license:bsd-3))))
