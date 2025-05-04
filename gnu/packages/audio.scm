;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016-2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016–2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 okapi <okapi@firemail.cc>
;;; Copyright © 2018, 2020, 2022-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2018, 2019, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2021 Thorsten Wilms <t_w_@freenet.de>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2022, 2023, 2024 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019 Hartmt Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Jonathan Frederickson <jonathan@terracrypt.net>
;;; Copyright © 2020, 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Arjan Adriaanse <arjan@adriaan.se>
;;; Copyright © 2022, 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2022, 2023 Simon Streit <simon@netpanic.org>
;;; Copyright © 2022 Andy Tai <atai@atai.org>
;;; Copyright © 2023 Sergiu Ivanov <sivanov@colimite.fr>
;;; Copyright © 2023 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Parnikkapore <poomklao@yahoo.com>
;;; Copyright © 2024 hapster <o.rojon@posteo.net>
;;; Copyright © 2024 mio <stigma@disroot.org>
;;; Copyright © 2024 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2025 Junker <dk@junkeria.club>
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
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

(define-module (gnu packages audio)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet) ; libmicrohttpd
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linphone)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3) ;taglib
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)  ;libsndfile, libsamplerate
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim) ;xxd
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system waf)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public opensles
  (package
    (name "opensles")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/KhronosGroup/OpenSL-ES-Registry")
         (commit "ea5104bf37bf525c25e6ae2386586048179d0fda")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5bm7h3ahz66f23i9abwc0y10agfkpksnj6y078x2nichq66h4f"))
       (patches
        (search-patches "opensles-add-license-file.patch"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'clean
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/etc"))
               (mkdir-p (string-append out "/include"))
               (mkdir-p (string-append out "/share"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES_IID.c")
                (string-append out "/etc/OpenSLES_IID.c"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES.h")
                (string-append out "/include/OpenSLES.h"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES_Platform.h")
                (string-append out "/include/OpenSLES_Platform.h"))
               (rename-file
                (string-append out "/api/1.1/README.txt")
                (string-append out "/share/README.txt"))
               (rename-file
                (string-append out "/LICENSE.txt")
                (string-append out "/share/LICENSE.txt"))
               (for-each delete-file-recursively
                         (list
                          (string-append out "/api")
                          (string-append out "/specs")))
               (for-each delete-file
                         (list
                          (string-append out "/CODE_OF_CONDUCT.md")
                          (string-append out "/index.php")
                          (string-append out "/README.md"))))
             #t)))))
    (synopsis "Embedded Audio Acceleration")
    (description "OpenSLES is a royalty-free, cross-platform,
hardware-accelerated audio API tuned for embedded systems.  It provides a
standardized, high-performance, low-latency method to access audio
functionality for developers of native applications on embedded mobile
multimedia devices, enabling straightforward cross-platform deployment of
hardware and software audio capabilities, reducing implementation effort, and
promoting the market for advanced audio.")
    (home-page "https://www.khronos.org/opensles/")
    (license (license:non-copyleft "file:///LICENSE.txt"))))

(define-public wildmidi
  (package
    (name "wildmidi")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Mindwerks/wildmidi")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08fbbsvw6pkwwqarjwcvdp8mq4zn5sgahf025hynwc6rvf4sp167"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No target
       #:configure-flags
       (list
        "-DWANT_ALSA=ON"
        "-DWANT_OSS=ON"
        "-DWANT_OPENAL=ON")))
    (inputs
     `(("alsa" ,alsa-lib)
       ("openal" ,openal)))
    (synopsis "Software Synthesizer")
    (description "WildMIDI is a simple software midi player which has a core
softsynth library that can be used with other applications.")
    (home-page "https://www.mindwerks.net/projects/wildmidi/")
    (license
     (list
      ;; Library.
      license:lgpl3+
      ;; Player.
      license:gpl3+))))

(define-public alsa-midi-latency-test
  (let ((version "0.0.5")
        (revision "0")
        (commit "07e43f8a1e6fd6d3bd97a00f2ee5afb74cb66f95"))
    (package
      (name "alsa-midi-latency-test")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/koppi/alsa-midi-latency-test")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b3xd4z7zx6mmh6q2q7wnyd0hzikny2cikwzhaab3q86b551vb9n"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                               ;there are no tests
        #:phases #~(modify-phases %standard-phases
                     (replace 'bootstrap
                       (lambda _
                         (invoke "sh" "./autogen.sh"))))))
      (native-inputs (list automake autoconf libtool))
      (inputs (list alsa-lib))
      (synopsis "Measure the roundtrip time of MIDI messages")
      (description
       "@code{alsa-midi-latency-test} measures the roundtrip time of a MIDI
message in the alsa subsystem of the Linux kernel using a high precision timer.
It calculates the worst case roundtrip time of all sent MIDI messages and
displays a histogram of the roundtrip time jitter.")
      (home-page "https://github.com/koppi/alsa-midi-latency-test")
      (license license:gpl2+))))

(define-public webrtc-audio-processing
  (package
    (name "webrtc-audio-processing")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://freedesktop.org/software/pulseaudio/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0xfvq5lxg612vfzk3zk6896zcb4cgrrb7fq76w9h40magz0jymcm"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags (if (target-x86-32?)
                                 #~(list "-Dc_args=-DPFFFT_SIMD_DISABLE")
                                 #~'())
           #:phases
           (if (or (target-x86-32?) (target-powerpc?))
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'apply-patches
                     (lambda _
                       (define (patch file)
                         (invoke "patch" "-p1" "--force" "-i" file))

                       ;; https://gitlab.freedesktop.org/pulseaudio/webrtc-audio-processing/-/issues/5
                       ;; TODO: Move to the 'patches' field of the origin on
                       ;; the next rebuild.
                       (patch #$(local-file
                                 (search-patch
                                  "webrtc-audio-processing-byte-order-pointer-size.patch")))
                       (patch #$(local-file
                                 (search-patch
                                  "webrtc-audio-processing-x86-no-sse.patch"))))))
               #~%standard-phases)))
    (native-inputs (list pkg-config))
    (inputs (list abseil-cpp))
    (synopsis "WebRTC's Audio Processing Library")
    (description "WebRTC-Audio-Processing library based on Google's
implementation of WebRTC.")
    (home-page
     "https://freedesktop.org/software/pulseaudio/webrtc-audio-processing/")
    (license (license:non-copyleft "file:///COPYING"))))

(define-public vo-aacenc
  (package
    (name "vo-aacenc")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0dhghm3c8pqrriwwyj5x9i0yf52fmdfijbgqqkvqvwarldvp86p5"))))
    (build-system gnu-build-system)
    (synopsis "VisualOn AAC Encoder")
    (description "VO-AACENC is the VisualOn implementation of Advanced Audio
Coding (AAC) encoder.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public tinyalsa
  (package
    (name "tinyalsa")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tinyalsa/tinyalsa")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p9khz3bdpdcrnc9p6w522a0ankdchj4nxd3ki41z9401rxmnljq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (synopsis "ALSA interfacing library")
    (description "TinyALSA is a small library to interface with ALSA in the
Linux kernel.")
    (home-page "https://github.com/tinyalsa/tinyalsa")
    (license (license:non-copyleft "file:///NOTICE"))))

(define-public libgme
  (package
    (name "libgme")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/mpyne/game-music-emu/"
                                  "downloads/game-music-emu-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "07857vdkak306d9s5g6fhmjyxk7vijzjhkmqb15s7ihfxx9lx8xb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                                ;no check target

       ;; XXX: Building with '-fsanitize=undefined' leads to embedded C++ STL
       ;; header file names in libgme.so, meaning that libgme retains a
       ;; reference to GCC.  Disable UBSAN to avoid that.
       #:configure-flags '("-DENABLE_UBSAN=OFF")))
    (home-page "https://bitbucket.org/mpyne/game-music-emu")
    (synopsis "Video game music file playback library")
    (description
     "Game-music-emu is a collection of video game music file emulators that
support the following formats and systems:
@table @code
@item AY
ZX Spectrum/Asmtrad CPC
@item GBS
Nintendo Game Boy
@item GYM
Sega Genesis/Mega Drive
@item HES
NEC TurboGrafx-16/PC Engine
@item KSS
MSX Home Computer/other Z80 systems (doesn't support FM sound)
@item NSF/NSFE
Nintendo NES/Famicom (with VRC 6, Namco 106, and FME-7 sound)
@item SAP
Atari systems using POKEY sound chip
@item SPC
Super Nintendo/Super Famicom
@item VGM/VGZ
Sega Master System/Mark III, Sega Genesis/Mega Drive, BBC Micro
@end table")
    (license (list license:lgpl2.1+
                   ;; demo and player directories are under the Expat license
                   license:expat))))

(define-public libopenmpt
  (package
    (name "libopenmpt")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://download.openmpt.org/archive/libopenmpt/src/"
                       "libopenmpt-" version "+release.autotools.tar.gz"))
       (sha256
        (base32 "0h86p8mnpm98vc4v6jbvrmm02fch7dnn332i26fg3a2s1738m04d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     (list doxygen perl pkg-config))
    (inputs
     `(("alsa" ,alsa-lib)
       ("flac" ,flac)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)
       ("sndfile" ,libsndfile)))
    (propagated-inputs
     ;; In Requires.private
     (list libogg libvorbis mpg123 zlib))
    (synopsis "Audio tracking library")
    (description "LibOpenMPT is a cross-platform C++ and C module playback
library.  It is based on the player code of the Open ModPlug Tracker project.")
    (home-page "https://openmpt.org/")
    (license (license:non-copyleft "file:///LICENSE"))))

(define-public libofa
  (package
    (name "libofa")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://storage.googleapis.com/"
                       "google-code-archive-downloads/v2/code.google.com/"
                       "musicip-libofa/" name "-" version ".tar.gz"))
       (sha256
        (base32 "184ham039l7lwhfgg0xr2vch2xnw1lwh7sid432mh879adhlc5h2"))
       (patches
        (search-patches
         "libofa-ftbfs-1.diff"
         "libofa-curl.diff"
         "libofa-ftbfs-2.diff"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl expat))
    (propagated-inputs
     (list fftw))
    (synopsis "Open Fingerprint Architecture")
    (description "LibOFA is an audio fingerprint library, created and provided
by MusicIP.")
    (home-page "https://code.google.com/archive/p/musicip-libofa/")
    (license license:gpl2+)))

(define-public libtimidity
  (package
    (name "libtimidity")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/" name "/files/"
                       name "/" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0sif6lxa058b1mg19zwjm8rl2sg8cg0443k4dgi65clz0jy7qi16"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))       ; XXX: LibTiMidity could not be initialised
    (native-inputs
     (list pkg-config))
    (inputs
     (list ao))
    (synopsis "MIDI to WAVE converter library")
    (description "LibTiMidity is a MIDI to WAVE converter library that uses
Gravis Ultrasound-compatible patch files to generate digital audio data from
General MIDI files.")
    (home-page "https://libtimidity.sourceforge.net/")
    (license
     ;; This project is dual-licensed.
     ;; Either of the following licenses can be exercised.
     (list
      license:lgpl2.1+
      license:artistic2.0))))

(define-public vo-amrwbenc
  (package
    (name "vo-amrwbenc")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0klx3nkidc6b8aawchpk19n3xlrzgnc046w4gd0rdqphw28v6ljn"))))
    (build-system gnu-build-system)
    (synopsis "Adaptive Multi Rate Codec")
    (description "VO-AMR is a library of VisualOn implementation of
Adaptive Multi Rate Narrowband and Wideband (AMR-NB and AMR-WB) speech codec.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public opencore-amr
  (package
    (name "opencore-amr")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0hfk9khz3by0119h3jdwgdfd7jgkdbzxnmh1wssvylgnsnwnq01c"))))
    (build-system gnu-build-system)
    (synopsis "Adaptive Multi Rate Codec")
    (description "OpenCore-AMR is a library of OpenCORE Framework
implementation of Adaptive Multi Rate Narrowband and Wideband
(AMR-NB and AMR-WB) speech codec.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public alsa-modular-synth
  (package
    (name "alsa-modular-synth")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/alsamodular/alsamodular"
                                  "/" version "/ams-" version ".tar.bz2"))
              (sha256
               (base32
                "0l8lwa4wfw98fgidzwkmg0zzq60djrpfg6znzrpfxhr9x23149ps"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           ;; We cannot use zita-alsa-pcmi (the successor of clalsadrv) due to
           ;; license incompatibility.
           clalsadrv
           fftw
           jack-1
           ladspa
           liblo
           qtbase-5))
    (native-inputs
     (list pkg-config qttools-5))
    (home-page "https://alsamodular.sourceforge.net/")
    (synopsis "Realtime modular synthesizer and effect processor")
    (description
     "AlsaModularSynth is a digital implementation of a classical analog
modular synthesizer system.  It uses virtual control voltages to control the
parameters of the modules.  The control voltages which control the frequency
e.g. of the VCO (Voltage Controlled Oscillator) and VCF (Voltage Controlled
Filter) modules follow the convention of 1V / Octave.")
    (license license:gpl2)))

(define-public aubio
  (package
    (name "aubio")
    (version "0.4.9")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://aubio.org/pub/aubio-" version ".tar.bz2"))
             (sha256
              (base32
               "1npks71ljc48w6858l9bq30kaf5nph8z0v61jkfb70xb9np850nl"))))
    (build-system waf-build-system)
    (arguments
     (list
      #:tests? #f                      ; no check target
      #:configure-flags
      #~(list
         (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
         "--enable-fftw3f"
         "--enable-jack"
         "--enable-sndfile"
         "--enable-samplerate"
         "--enable-avcodec")
       #:phases
       '(modify-phases %standard-phases
          (add-after 'unpack 'python3.11-compatibility
            (lambda _
              (substitute* '("waflib/Context.py"
                             "waflib/ConfigSet.py")
                (("'rU'") "'r'")))))))
    (inputs
     (list jack-2
           libsndfile
           libsamplerate
           ffmpeg-4                     ;for libavcodec
           fftwf))
    (native-inputs
     (list pkg-config))
    (home-page "https://aubio.org/")
    (synopsis "Library for audio labelling")
    (description
     "aubio is a tool designed for the extraction of annotations from audio
signals.  Its features include segmenting a sound file before each of its
attacks, performing pitch detection, tapping the beat and producing MIDI
streams from live audio.")
    (license license:gpl3+)))

(define-public dsp
  (package
    (name "dsp")
    (version "1.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bmc0/dsp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iksmianwig7w78hqip2a8yy6r63sv8cv9pis8qxny6w1xap6njb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;no tests
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (invoke "sh" "configure"
                      (string-append "--prefix=" #$output)
                      "--disable-pulse"))))))
    (inputs
     (list alsa-lib
           ao
           ffmpeg
           ladspa
           libmad
           libsndfile
           fftw
           fftwf
           zita-convolver))
    (native-inputs
     (list libtool pkg-config))
    (home-page "https://github.com/bmc0/dsp")
    (synopsis "Audio processing program with an interactive mode")
    (description
     "dsp is an audio processing program with an interactive mode.")
    (license license:isc)))

(define-public qm-dsp
  (package
    (name "qm-dsp")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/c4dm/qm-dsp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vkb1xr2hjcaw88gig7rknlwsx01lm0w94d2z0rk5vz9ih4fslvv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list #$(string-append "-f" "build/"
                               (match (or (%current-target-system)
                                          (%current-system))
                                 ("x86_64-linux" "linux/Makefile.linux64")
                                 ("i686-linux"   "linux/Makefile.linux32")
                                 (target
                                  (if (string-suffix? "-mingw32" target)
                                      "mingw32/Makefile.mingw32"
                                      "general/Makefile.inc"))))
              (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)          ;no configure script
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((lib (string-append #$output "/lib"))
                      (include (string-append #$output "/include")))
                 (install-file "libqm-dsp.a" lib)
                 (mkdir-p include)
                 (for-each (lambda (file)
                             (unless (or (string-prefix? "./build" file)
                                         (string-prefix? "./include" file))
                               (install-file file (string-append include "/"
                                                                 (dirname file)))))
                           (find-files "." "\\.h$"))))))
       #:test-target "tests"))
    (home-page "https://code.soundsoftware.ac.uk/projects/qm-dsp")
    (synopsis "C++ library of functions for DSP and Music Informatics purposes")
    (description
     "QM-DSP is a C++ library of functions for DSP and Music Informatics
purposes developed at Queen Mary, University of London.")
    (license license:gpl2+)))

(define-public jamesdsp
  (package
    (name "jamesdsp")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/Audio4Linux/JDSP4Linux")
          (commit version)
          ;; Recurse GraqhicEQWidget, FlatTabWidget, LiquidEqualizerWidget and
          ;; EELEditor.
          (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17vx12kbvwxvb69vzrlb82mrgf6sl3plyk71g9f39p49ialdsnbr"))
       (modules '((guix build utils)))
       (snippet
        ;; Unbundle 3rd party libraries.
        '(begin
           ;; Delete the bundled 3rd party libraries.
           (for-each delete-file-recursively
            (list "3rdparty"
                  "src/subprojects/EELEditor/3rdparty"
                  "src/subprojects/EELEditor/QCodeEditor"
                  "src/subprojects/EELEditor/src/EELEditor-Linker.pri"))
           (with-directory-excursion "src"
             (substitute* "src.pro"
               ;; Do not use bundled 3rd party libraries.
               ((".*3rdparty.*") "")
               ;; Link required libraries from system.
               (("-ldl")
                (string-join '("-ldl"
                               "-lasync++"
                               "-lQCodeEditor"
                               "-lqcustomplot"
                               "-lqtadvanceddocking-qt6"
                               "-lqtcsv"
                               "-lwaf")
                               " ")))
             ;; Fix including WAF headers.
             (substitute* "MainWindow.cpp"
                       (("<Animation") "<WAF/Animation"))
             ;; Do not use resources from the bundled docking-system.
             (substitute* '("interface/fragment/AppManagerFragment.ui")
               ((".*location.*3rdparty.*") "")
               ((" resource=.*>") ">"))
             (with-directory-excursion "subprojects/EELEditor/src"
               ;; Do not use bundled QCodeEditor and docking-system.
               (substitute* "EELEditor.pri"
                 ((".*(QCodeEditor|docking-system).*") ""))
               ;; Do not link to bundled docking-system.
               (substitute* "src.pro"
                 ((".*EELEditor-Linker.*") ""))
               ;; Fix including headers from the system.
               (substitute* (find-files "." "\\.(cpp|h)$")
                 (("#include <Dock") "#include <qtadvanceddocking-qt6/Dock")
                 (("#include <FloatingDock")
                  "#include <qtadvanceddocking-qt6/FloatingDock")
                 (("#include <QSyntaxStyle")
                  "#include <QCodeEditor/QSyntaxStyle")
                 (("#include <QStyleSyntaxHighlighter")
                  "#include <QCodeEditor/QStyleSyntaxHighlighter")
                 (("#include <QHighlightRule")
                  "#include <QCodeEditor/QHighlightRule")
                 (("#include <QLanguage") "#include <QCodeEditor/QLanguage")
                 (("#include <QCodeEditor\\.hpp")
                  "#include <QCodeEditor/QCodeEditor.hpp"))))))
       (patches (search-patches "jamesdsp-fix-bulid-on-pipewire-1.4.0.patch"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f ;no tests
           #:phases
           #~(modify-phases %standard-phases
               ;; Configure using qmake.
               (replace 'configure
                 (lambda* (#:key inputs #:allow-other-keys)
                   (invoke "qmake" (string-append "PREFIX=" #$output))))
               (add-after 'install 'install-icon
                 (lambda _
                   (let ((pixmaps (string-append #$output "/share/pixmaps")))
                     (mkdir-p pixmaps)
                     (copy-file "resources/icons/icon.png"
                                (string-append pixmaps "/jamesdsp.png")))))
               (add-after 'install-icon 'create-desktop-entry-file
                 (lambda _
                   (make-desktop-entry-file
                    (string-append #$output
                                  "/share/applications/jamesdsp.desktop")
                    #:name "JamesDSP"
                    #:comment "Audio effect processor"
                    #:keywords '("equalizer" "audio" "effect")
                    #:categories '("AudioVideo" "Audio")
                    #:exec (string-append #$output "/bin/jamesdsp")
                    #:icon (string-append #$output "/share/pixmaps/jamesdsp.png")
                    #:startup-notify #f))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list asyncplusplus
           glibmm-2.66
           libarchive
           pipewire
           qcodeeditor
           qcustomplot
           qt-advanced-docking-system
           qtcsv
           qtpromise
           qtsvg
           qtwidgetanimationframework))
    (home-page "https://github.com/Audio4Linux/JDSP4Linux")
    (synopsis "Audio effect processor for PipeWire and PulseAudio clients")
    (description "JamesDSP is an audio effect processor for PipeWire and
PulseAudio clients, featuring:
@itemize
@item Automatic bass boost: Frequency-detecting bass-boost
@item Automatic dynamic range compressor: automated multiband dynamic range
 adjusting effect
@item Complex reverberation IIR network (Progenitor 2)
@item Interpolated FIR equalizer with flexible bands
@item Arbitrary response equalizer (also known as GraphicEQ from EqualizerAPO)
@item AutoEQ database integration (requires network connection)
@item Partitioned convolver (Auto segmenting convolution): Mono, stereo,
 full/true stereo (LL, LR, RL, RR) impulse response
@item Crossfeed: Realistic surround effects
@item Soundstage wideness: A multiband stereo wideness controller
@item ViPER-DDC: Parametric equalization on audio and creating VDC input files
@item Analog modeling: An aliasing-free even harmonic generator
@item Output limiter
@item Scripting engine: Live programmable DSP using the EEL2 scripting language
@item Scripting IDE featuring syntax highlighting, basic code completion,
 dynamic code outline window, console output support and detailed error
 messages with inline code highlighting
@end itemize")
    (license license:gpl3+)))

(define ardour-bundled-media
  (origin
    (method url-fetch)
    (uri "http://stuff.ardour.org/loops/ArdourBundledMedia.zip")
    (sha256
     (base32
      "0k135sm559yywfidrya7h5cddwqa2p2abhimrar2khydf43f03d0"))))

(define-public ardour
  (package
    (name "ardour")
    (version "8.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.ardour.org/ardour/ardour.git")
                    (commit version)))
              (snippet
               ;; Ardour expects this file to exist at build time.  The revision
               ;; is the output of
               ;;    git describe HEAD | sed 's/^[A-Za-z]*+//'
               `(call-with-output-file
                    "libs/ardour/revision.cc"
                  (lambda (port)
                    (format port ,(string-append "#include \"ardour/revision.h\"
namespace ARDOUR { const char* revision = \"" version "\" ; const char* date = \"\"; }")))))
              (sha256
               (base32
                "11aczxkr5rz9lsxrsbwxaj4yr2di7agbqmrxs6pvwi549fiqv1yb"))
              (file-name (git-file-name name version))))
    (build-system waf-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "--optimize"
             "--no-phone-home"          ;don't contact ardour.org
             "--no-ytk"                 ;don't use bundled GTK2
             "--freedesktop"            ;build .desktop file
             "--test"                   ;build unit tests
             "--use-external-libs")     ;use system libraries
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'set-rpath-in-LDFLAGS
             (lambda _
               (let ((libdir (string-append #$output
                                            "/lib/ardour"
                                            #$(version-major version))))
                 (substitute* "wscript"
                   (("linker_flags = \\[\\]")
                    (string-append "linker_flags = [\""
                                   "-Wl,-rpath="
                                   libdir ":"
                                   libdir "/backends" ":"
                                   libdir "/engines" ":"
                                   libdir "/panners" ":"
                                   libdir "/surfaces" ":"
                                   libdir "/vamp" "\"]"))))))
           (add-after 'build 'build-i18n
             (lambda _
               (invoke "python" "waf" "i18n")))
           (add-after 'install 'install-freedesktop-files
             (lambda _
               (let ((share (string-append #$output "/share"))
                     (ver #$(version-major version)))
                 (for-each
                  (lambda (size)
                    (let ((dir (string-append share "/icons/hicolor/"
                                              size "x" size "/apps")))
                      (mkdir-p dir)
                      (copy-file
                       (string-append "gtk2_ardour/resources/Ardour-icon_"
                                      size "px.png")
                       (string-append dir "/ardour" ver ".png"))))
                  '("16" "22" "32" "48" "256"))
                 (install-file (string-append "build/gtk2_ardour/ardour"
                                              ver ".desktop")
                               (string-append share "/applications/"))
                 (install-file (string-append "build/gtk2_ardour/ardour"
                                              ver ".appdata.xml")
                               (string-append share "/appdata/")))))
           (add-after 'install 'install-man-page
             (lambda _
               (install-file "ardour.1" (string-append #$output
                                                       "/share/man/man1"))))
           (add-after 'install 'install-bundled-media
             (lambda _
               (invoke "unzip" "-d" (string-append #$output
                                                   "/share/ardour"
                                                   #$(version-major version)
                                                   "/media/")
                       #$ardour-bundled-media))))
       #:test-target "test"))
    (inputs
     (list alsa-lib
           atkmm
           aubio
           cairomm
           curl
           dbus
           eudev
           fftw
           fftwf
           flac
           fluidsynth
           glibmm
           gtkmm-2
           hicolor-icon-theme
           hidapi
           jack-2
           libarchive
           libart-lgpl
           libgnomecanvasmm
           liblo
           libltc
           libogg
           libsamplerate
           libsndfile
           libusb
           libvorbis
           libwebsockets
           libxinerama
           libxml2
           libxrandr
           lilv
           lrdf
           lv2
           openssl ; Required by libwebsockets.
           pangomm
           python-rdflib
           pulseaudio
           qm-dsp
           readline
           redland
           rubberband
           serd
           sord
           soundtouch
           sratom
           suil
           taglib
           vamp))
    (native-inputs
     (list boost
           cppunit
           gettext-minimal
           itstool
           perl
           pkg-config
           unzip))
    (home-page "https://ardour.org")
    (synopsis "Digital audio workstation")
    (description
     "Ardour is a multi-channel digital audio workstation, allowing users to
record, edit, mix and master audio and MIDI projects.  It is targeted at audio
engineers, musicians, soundtrack editors and composers.")
    (license (list license:gpl2+
                   license:cc0 ;used by MIDI Beats
                   license:expat)))) ;used by MIDI Chords and Progressions

(define-public audacity
  (package
    (name "audacity")
    (version "3.5.1")            ;for ffmpeg 6 support
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/audacity/audacity")
             (commit (string-append "Audacity-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11sjyz6hxsr5dnndkkkiq7arjxvjl1sycl151xq3a3ggakgql3y1"))
       (patches (search-patches "audacity-ffmpeg-fallback.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (for-each
            (lambda (dir)
              (delete-file-recursively (string-append "lib-src/" dir)))
            '("libsoxr" "libvamp" "lv2" "soundtouch" "sqlite" "twolame"
              ;; FIXME: these libraries have not been packaged yet:
              ;; "libnyquist"
              ;; "libscorealign"
              ;; "portburn"
              ;; "portsmf"
              ;; "portmixer"

              ;; FIXME: we have this library, but it differs in that the Slide
              ;; class does not have a member "getInverseStretchedTime".
              ;; "sbsms"
              ))))))
    (build-system cmake-build-system)
    (inputs
     (list wxwidgets
           gtk+
           alsa-lib
           jack-1
           expat
           lame
           flac
           ffmpeg
           libid3tag
           libjpeg-turbo
           ;;("libsbsms" ,libsbsms)         ;bundled version is modified
           libsndfile
           mpg123
           opusfile
           rapidjson
           soundtouch
           soxr                         ;replaces libsamplerate
           sqlite
           twolame
           vamp
           libvorbis
           lv2
           lilv                         ;for lv2
           suil                         ;for lv2
           portaudio
           portmidi
           wavpack))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal              ;for msgfmt
           libtool
           pkg-config
           python
           which))
    (arguments
     `(#:configure-flags
       (list
        "-Daudacity_conan_enabled=off"
        "-Daudacity_lib_preference=system"
        ;; Disable support for VST 3 SDK, which is not yet in Guix (and has
        ;; a dubious licensing agreement despite GPL code).
        "-Daudacity_has_vst3=off"
        ;; TODO: enable this flag once we've packaged all dependencies
        ;; "-Daudacity_obey_system_dependencies=on"
        ;; disable crash reports, updates, ..., anything that phones home
        "-Daudacity_has_networking=off"
        ;; When building from Git — even from a release tag — this is undefined,
        ;; and Audacity assumes that is is an ‘alpha’ version and includes debug
        ;; symbols and extra code.  Force level 2, ‘release’.
        "-DAUDACITY_BUILD_LEVEL=2")
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
       #:modules
       ((guix build utils)
        (guix build cmake-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/CMakeLists.txt"
               ;; Despite the name, this script breaks rpath.  Don't run it.
               (("install.*linux/fix_rpath\\.cmake.*")
                "")
               (("-Wl,--disable-new-dtags") "-Wl,--enable-new-dtags"))))
         (add-after 'unpack 'use-upstream-headers
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("libraries/lib-files/FileNames.cpp")
               (("\"/usr/include/linux/magic.h\"") "<linux/magic.h>"))))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))
       ;; The test suite is not "well exercised" according to the developers,
       ;; and fails with various errors.  See
       ;; <http://sourceforge.net/p/audacity/mailman/message/33524292/>.
       #:tests? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "AUDACITY_MODULES_PATH")
            (files '("lib/audacity/modules")))
           (search-path-specification
            (variable "AUDACITY_PATH")
            (files '("share/audacity")))))
    (home-page "https://www.audacityteam.org/")
    (synopsis "Software for recording and editing sounds")
    (description
     "Audacity is a multi-track audio editor designed for recording, playing
and editing digital audio.  It features digital effects and spectrum analysis
tools.")
    (license license:gpl2+)))

(define-public tenacity
  (package
    (name "tenacity")
    (version "1.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/tenacityteam/tenacity")
             (commit (string-append "v" version))
             ;; TODO Unbundle vcpkg when packaged in Guix.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wphm494517zmnhgrmzlzld2j4bfl2c73qr61nrss90410xxs2fs"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:modules
      '((guix build utils)
        (guix build cmake-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-upstream-headers
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("libraries/lib-files/FileNames.cpp")
                (("\"/usr/include/linux/magic.h\"") "<linux/magic.h>"))))
          (add-after 'unpack
              'i-spy-with-my-little-eye-something-in-the-wrong-folder
            (lambda _
              (symlink (string-append (getcwd) "/images")
                       "src/images")

              (symlink (string-append (getcwd) "/images")
                       "src/tracks/images"))
          )
          (add-after 'unpack 'fix-cmake-rpath
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "CMakeLists.txt"
                (("\\$ORIGIN/\\.\\./\\$\\{_PKGLIB\\}")
                 (string-append (assoc-ref outputs "out") "/lib/tenacity"))
                (("CMAKE_BUILD_WITH_INSTALL_RPATH [A-Z]*")
                 "CMAKE_BUILD_WITH_INSTALL_RPATH TRUE")
                (("CMAKE_INSTALL_RPATH_USE_LINK_PATH [A-Z]*")
                 "CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE"))
              (substitute* "src/CMakeLists.txt"
                ;; Despite the name, this script breaks rpath.  Don't run it.
                (("install.*linux/fix_rpath\\.cmake.*")
                 "")
                (("-Wl,--disable-new-dtags") "-Wl,--enable-new-dtags"))))
          (replace 'configure
            (lambda args
              (define %configure (assoc-ref %standard-phases 'configure))
              (with-exception-handler
                  (lambda (error)
                    (unless (invoke-error? error)
                      (raise error))
                    ;; Have you tried turning it off and on again?
                    (apply invoke (invoke-error-program error)
                           (invoke-error-arguments error)))
                (lambda ()
                  (apply %configure args))
                #:unwind? #t)))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))
      ;; Test suite?  Which test suite?
      #:tests? #f))
    (inputs
     (list wxwidgets
           gtk+
           alsa-lib
           jack-1
           expat
           lame
           flac
           ffmpeg
           libid3tag
           libjpeg-turbo
           ;;("libsbsms" ,libsbsms)         ;bundled version is modified
           libsndfile
           mpg123
           soundtouch
           soxr ;replaces libsamplerate
           sqlite
           twolame
           vamp
           libebml
           libmatroska
           libvorbis
           lv2
           lilv ;for lv2
           suil ;for lv2
           portaudio
           portmidi
           wavpack))
    (native-inputs
     (list gettext-minimal              ;for msgfmt
           libtool
           pkg-config
           python
           which))
    (native-search-paths
     (list (search-path-specification
            (variable "TENACITY_MODULES_PATH")
            (files '("lib/tenacity/modules")))
           (search-path-specification
            (variable "TENACITY_PATH")
            (files '("share/tenacity")))))
    (home-page "https://tenacityaudio.org/")
    (synopsis "Software for recording and editing sounds")
    (description
     "Tenacity is a multi-track audio editor designed for recording, playing
and editing digital audio.  It features digital effects and spectrum analysis
tools.")
    (license license:gpl2+)))

(define-public audiofile
  (package
    (name "audiofile")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://audiofile.68k.org/audiofile-" version ".tar.gz"))
       (sha256
        (base32 "0rb927zknk9kmhprd8rdr4azql4gn2dp75a36iazx2xhkbqhvind"))
       (patches
        ;; CVE references according to nixpgs
        (search-patches
         "audiofile-fix-datatypes-in-tests.patch"
         "audiofile-fix-sign-conversion.patch"
         "audiofile-hurd.patch"
         "audiofile-CVE-2015-7747.patch"
         ;; CVE-2017-6829:
         "audiofile-Fix-index-overflow-in-IMA.cpp.patch"
         ;; CVE-2017-6827, CVE-2017-6828, CVE-2017-6832, CVE-2017-6835,
         ;; CVE-2017-6837:
         "audiofile-check-number-of-coefficients.patch"
         ;; CVE-2017-6839:
         "audiofile-overflow-in-MSADPCM.patch"
         ;; CVE-2017-6830, CVE-2017-6834, CVE-2017-6836, CVE-2017-6838:
         "audiofile-multiply-overflow.patch"
         "audiofile-function-signature.patch"
         ;; CVE-2017-6831:
         "audiofile-Fail-on-error-in-parseFormat.patch"
         ;; CVE-2017-6833:
         "audiofile-division-by-zero.patch"
         "audiofile-CVE-2018-13440.patch"
         "audiofile-CVE-2018-17095.patch"))))
    (properties `((lint-hidden-cve . ("CVE-2017-6829"

                                      "CVE-2017-6827" "CVE-2017-6828"
                                      "CVE-2017-6832" "CVE-2017-6835"
                                      "CVE-2017-6837"

                                      "CVE-2017-6839"

                                      "CVE-2017-6830" "CVE-2017-6834"
                                      "CVE-2017-6836" "CVE-2017-6838"

                                      "CVE-2017-6831" "CVE-2017-6833"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib))
    (home-page "https://audiofile.68k.org/")
    (synopsis "Library to handle various audio file formats")
    (description "This is an open-source version of SGI's audiofile library.
It provides a uniform programming interface for processing of audio data to
and from audio files of many common formats.

Currently supported file formats include AIFF/AIFF-C, WAVE, and NeXT/Sun
.snd/.au, BICS, and raw data.  Supported compression formats are currently
G.711 mu-law and A-law.")
    (license license:lgpl2.1+)))

(define-public autotalent
  (package
    (name "autotalent")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tombaran.info/autotalent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1n04qm66f14195ly6gsy3ra7v2j7zad5n19d8dwfmh0qs6h9hphh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/lib64/ladspa")
                (string-append (assoc-ref outputs "out") "/lib/ladspa")))
             #t)))))
    (inputs
     (list ladspa))
    (home-page "http://tombaran.info/autotalent.html")
    (synopsis "Pitch-correction LADSPA audio plugin")
    (description
     "Autotalent is a LADSPA plugin for real-time pitch-correction.  Among its
controls are allowable notes, strength of correction, LFO for vibrato and
formant warp.")
    ;; All code except the FFT routine is licensed under GPLv2+.
    ;; The FFT routine is under BSD-3.
    (license license:gpl2+)))

(define-public azr3
  (let ((commit "3391a0a509e7fa3fb46c7627fd5979b67e468038")
        (revision "1"))
    (package
      (name "azr3")
      (version (git-version "1.2.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/ll-plugins/azr3-jack.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09wy0z4kiid7mwf5b5j8rzzgxafi4mg88xs550n7864p0n351chx"))
                (patches (search-patches "azr3.patch"
                                         "azr3-remove-lash.patch"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                       ; no check target
        #:make-flags
        #~(list "LV2PEG=ttl2c"
                (string-append "prefix=" #$output)
                (string-append "pkgdatadir=" #$output "/share/azr3-jack"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'bootstrap
              (lambda _
                (call-with-output-file "Makefile.config"
                  (lambda (port) (display "" port)))
                (substitute* "Makefile"
                  (("^PACKAGE_VERSION =.*")
                   (string-append "PACKAGE_VERSION = \"" #$version "\"\n")))))
            (add-before 'install 'fix-timestamp
              (lambda _
                (let ((early-1980 315619200)) ; 1980-01-02 UTC
                  (utime "azr3.1" early-1980 early-1980)))))))
      (inputs
       (list gtkmm-2 jack-2 lvtk))
      (native-inputs
       (list pkg-config))
      (home-page "https://ll-plugins.nongnu.org/azr3/")
      (synopsis "Tonewheel organ synthesizer")
      (description
       "AZR-3 is a port of the free VST plugin AZR-3.  It is a tonewheel organ
with drawbars, distortion and rotating speakers.  The organ has three
sections, two polyphonic sections with nine drawbars each and one monophonic
bass section with five drawbars.  A standalone JACK application and LV2
plugins are provided.")
      (license license:gpl2))))

(define-public bankstown-lv2
  (package
    (name "bankstown-lv2")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bankstown-lv2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bcrn0b4b9v1mksaldhrdb6ncqlwldfwqxjlfp4gcpvl661qdmcb"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-biquad" ,rust-biquad-0.4)
                       ("rust-lv2" ,rust-lv2-0.6))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "LIBDIR" (string-append (assoc-ref outputs "out") "/lib"))
              (invoke "make" "install"))))))
    (home-page "https://github.com/chadmed/bankstown")
    (synopsis "Barebones, fast LV2 bass enhancement plugin.")
    (description
     "This package provides a barebones, fast LV2 bass enhancement plugin.")
    (license license:expat)))

(define-public calf
  (package
    (name "calf")
    (version "0.90.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/calf-studio-gear/calf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p4zqzr7spy3jjsmy6h7n5lsyqqyh23bswk1r3kims50b102xhxd"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #false)) ;there is no test target
    (inputs
     (list fluidsynth
           expat
           glib
           gtk+-2
           cairo
           jack-2
           lv2
           ladspa
           fftw))
    (native-inputs
     (list pkg-config))
    (home-page "https://calf-studio-gear.org/")
    (synopsis "Audio plug-in pack for LV2 and JACK environments")
    (description
     "Calf Studio Gear is an audio plug-in pack for LV2 and JACK environments.
The suite contains lots of effects (delay, modulation, signal processing,
filters, equalizers, dynamics, distortion and mastering effects),
instruments (SF2 player, organ simulator and a monophonic synthesizer) and
tools (analyzer, mono/stereo tools, crossovers).")
    ;; calfjackhost is released under GPLv2+
    ;; The plugins are released under LGPLv2.1+
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public caps-plugins-lv2
  (package
    (name "caps-plugins-lv2")
    (version "0.9.26")
    (source
     (origin
       ;; The Github project hasn't tagged a release.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moddevices/caps-lv2.git")
             (commit "5d52a0c6e8863c058c2aab2dea9f901a90d96eb9")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hdl7n3ra5gqgwkdfqkw8dj9gb1cgb76bn1v91w06d2w4lj9s8xa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files "plugins" "Makefile")
               (("/usr/local")(assoc-ref outputs "out")))
             #t)))))
    (inputs
     (list lv2))
    ;; home-page of the original LADSPA version: http://quitte.de/dsp/caps.html
    (home-page "https://github.com/moddevices/caps-lv2")
    (synopsis "LV2 port of the CAPS audio plugin collection")
    (description
     "LV2 port of CAPS, a collection of audio plugins comprising basic virtual
guitar amplification and a small range of classic effects, signal processors and
generators of mostly elementary and occasionally exotic nature.")
    (license license:gpl3+)))

(define-public chow-tape-model
  (package
    (name "chow-tape-model")
    (version "2.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jatinchowdhury18/AnalogTapeModel")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qrqhlfzc2m5iwrkfzb53x8hll2ndn1fygh1mwn11shqmy5qgf2s"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      (let ((libs "-lX11 -lXext -lXcursor -lXinerama -lXrandr"))
        `(list "-DBUILD_HEADLESS=ON"
               ,(string-append "-DCMAKE_SHARED_LINKER_FLAGS=" libs)
               ,(string-append "-DCMAKE_EXE_LINKER_FLAGS=" libs)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-manual
            (lambda _
              (with-directory-excursion "Manual"
                (invoke "make" "all"))))
          (add-after 'build-manual 'install-manual
            (lambda _
              (with-directory-excursion "Manual"
                (install-file "ChowTapeManual.pdf"
                              (string-append #$output:doc "/share/doc/")))))
          (add-after 'install-manual 'chdir
            (lambda _ (chdir "Plugin")))
          (replace 'check
            (lambda* (#:key tests? build-type #:allow-other-keys)
              (when tests?
                (with-directory-excursion
                    (string-append "Source/Headless/ChowTapeModel_Headless_artefacts/" build-type)
                  (invoke "./ChowTapeModel_Headless" "--unit-tests" "--all")))))
          (replace 'install
            (lambda* (#:key build-type #:allow-other-keys)
              (with-directory-excursion
                  (string-append "CHOWTapeModel_artefacts/" build-type)
                (mkdir-p (string-append #$output:lv2 "/lib/lv2/"))
                (mkdir-p (string-append #$output "/bin/"))
                (install-file "Standalone/CHOWTapeModel"
                              (string-append #$output "/bin/"))
                (install-file "CLAP/CHOWTapeModel.clap"
                              (string-append #$output:clap "/lib/clap/"))
                (copy-recursively "LV2/CHOWTapeModel.lv2"
                                  (string-append #$output:lv2
                                                 "/lib/lv2/CHOWTapeModel.lv2"))
                (copy-recursively "VST3/CHOWTapeModel.vst3"
                                  (string-append #$output:vst3
                                                 "/lib/vst3/CHOWTapeModel.vst3"))))))))
    (outputs '("out" "doc" "clap" "lv2" "vst3"))
    (inputs
     (list alsa-lib
           freeglut
           freetype
           jack-2
           libxcursor
           libxext
           libxinerama
           libxrandr
           lv2))
    (native-inputs
     (list pkg-config
           (texlive-updmap.cfg
            (list texlive-geometry
                  texlive-xetex
                  texlive-collection-pictures))))
    (home-page "https://chowdsp.com/products.html")
    (synopsis "Physical modeling for analog tape machines")
    (description
     "CHOW Tape is an analog tape machine physical model, originally based on
the Sony TC-260.  The current version can be used to emulate a wide variety of
reel-to-reel tape machines.")
    (license license:gpl3)))

(define-public iir
  (package
    (name "iir")
    (version "1.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/berndporr/iir1")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fqxn0qlvykpk9hiliivmkjjcz3g1bp83yd0zfm82r14abkjbj2g"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'delete-static-library
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (delete-file (string-append out "/lib/libiir_static.a"))))))))
    (home-page "https://berndporr.github.io/iir1/")
    (synopsis
     "Real-time C++ @acronym{IIR, infinite impulse response} filter library")
    (description
     "This C++ library implements the Butterworth, RBJ, and Chebychev
@acronym{IIR, infinite impulse response} filters.  Samples are processed one by
one, in real time.  It can easily import coefficients generated with Python
(@code{scipy}).  It also avoids memory leaks by allocating memory at compile
time, using templates, instead of calling @code{malloc()} or @code{new}.")
    (license license:expat)))

(define-public infamous-plugins
  (package
    (name "infamous-plugins")
    (version "0.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ssj71/infamousPlugins")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ay66lly6bgqr3nzb0y4b29rgl5y1slk6wf73kr3xiw2p62bh582"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-compiler-flags
           (lambda _
             (substitute* (find-files "." "CMakeLists.txt")
               (("-msse2 -mfpmath=sse") "")))))))
    (inputs
     (list cairo fftwf lv2 ntk zita-resampler))
    (native-inputs
     (list pkg-config))
    (home-page "https://ssj71.github.io/infamousPlugins")
    (synopsis "LV2 plugins for live use")
    (description
     "The infamous plugins are a collection of LV2 audio plugins for live
performances.  The plugins include a cellular automaton synthesizer, an
envelope follower, distortion effects, tape effects and more.")
    (license license:gpl2+)))

(define-public omins-lv2
  (let ((commit "058f341053067b69a84d4081107fda5058290ff9")
	(revision "1"))
    (package
      (name "omins-lv2")
      ;; No release despite being perfectly usable. 0.0.0 seems to be the only
      ;; version the author ever specified:
      (version (git-version "0.0.0" revision commit))
      (source
       (origin (method git-fetch)
	       (uri
	        (git-reference
	         (url "https://git.drobilla.net/cgit.cgi/omins.lv2.git/")
	         (commit commit)))
	       (file-name (git-file-name name version))
	       (sha256
	        (base32
                 "01hnx4hhbz3ap3bw15s42q4q1mw1mhdjwygq4550wvjfg6k4ga8w"))))
      (build-system waf-build-system)
      (arguments (list #:tests? #false)) ;There are no tests.
      (inputs (list lv2))
      (native-inputs (list pkg-config))
      (home-page "https://git.drobilla.net/cgit.cgi/omins.lv2.git/")
      (synopsis "LV2 audio plugins for modular synthesis")
      (description
       "Omins-lv2 is a small collection of LV2 audio plugins for modular
synthesis.")
      (license license:gpl2+))))

(define-public snapcast
  (package
    (name "snapcast")
    (version "0.29.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/badaix/snapcast")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1960xp54vsndj9vvc03kx9kg9phdchdgrfghhvcp2b0nfq2qcqqm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no included tests
    (inputs
     (list boost
           libvorbis
           soxr
           alsa-lib
           avahi
           pulseaudio
           flac
           opus))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/badaix/snapcast")
    (synopsis "Synchronous multiroom audio player")
    (description
     "Snapcast is a multi-room client-server audio player.  Clients are time
synchronized with the server to play synced audio.")
    (license license:gpl3+)))

(define-public swh-plugins
  (package
    (name "swh-plugins")
    (version "0.4.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/ladspa")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c98z2xxz9pgcb4dg99gz8qrylh5cnag0j18a52d88ifsy24isvq"))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal ;for autopoint
           libtool
           perl
           pkg-config
           which))
    (inputs
     (list fftwf perl-xml-parser))
    (build-system gnu-build-system)
    (home-page "http://plugin.org.uk")
    (synopsis "The SWH Plugins package for the LADSPA plugin system")
    (description "This package provides Steve Harris's LADSPA plugins.")
    (license license:gpl2+)))

(define-public swh-plugins-lv2
  (package
    (name "swh-plugins-lv2")
    (version "1.0.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/lv2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y7nnww864mm4k6ayy2lhcws3wlbhb2gkyjbrwk921fvc18qk9mz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-after 'unpack 'patch-makefile-and-enter-directory
           ;; The default install target doesn't install, but the
           ;; "install-system" target does.
           (lambda _
             (substitute* "Makefile"
               (("install:") "install: install-system"))
             #t)))))
    (inputs
     (list lv2 fftwf))
    (native-inputs
     (list libxslt pkg-config))
    (home-page "http://plugin.org.uk")
    (synopsis "SWH plugins in LV2 format")
    (description
     "Swh-plugins-lv2 is a collection of audio plugins in LV2 format.  Plugin
classes include: dynamics (compressor, limiter), time (delay, chorus,
flanger), ringmodulator, distortion, filters, pitchshift, oscillators,
emulation (valve, tape), bit fiddling (decimator, pointer-cast), etc.")
    (license license:gpl3+)))

(define-public libdjinterop
  (package
    (name "libdjinterop")
    (version "0.24.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xsco/libdjinterop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g0yfmwmrkvrkvynx84bays6jph3wq2hq1md5ylr7n5a8g0c17hn"))))
    (build-system cmake-build-system)
    (native-inputs
     (list boost pkg-config))
    (inputs
     (list sqlite zlib))
    (home-page "https://github.com/xsco/libdjinterop")
    (synopsis "C++ library for access to DJ record libraries")
    (description
     "@code{libdjinterop} is a C++ library that allows access to database
formats used to store information about DJ record libraries.")
    (license license:lgpl3+)))

(define-public tao
  (package
    (name "tao")
    (version "1.0-beta-10May2006")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/taopm/Tao/"
                                  "tao-" version "/"
                                  "tao-" version ".tar.gz"))
              (sha256
               (base32
                "156py3g6mmglldfd0j76bn7n242hdwf49diaprjpj7crp8vgf2pz"))
              (patches
               (search-patches "tao-add-missing-headers.patch"
                               "tao-fix-parser-types.patch"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("SHELL=/bin/sh") "")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("TAO_RELEASE=-beta")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-references
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "user-scripts/tao.in"
               (("taoparse")
                (string-append (assoc-ref outputs "out") "/bin/taoparse"))
               (("grep") (which "grep"))
               (("sed -f \\$distdir/user-scripts/")
                (string-append (which "sed") " -f $distdir/"))
               (("distdir=.*")
                (string-append "distdir="
                               (assoc-ref outputs "out") "/share/tao")))))
         (add-after 'install 'install-extra-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/tao/"))
                    (inc (string-append out "/include/tao/")))
               (mkdir-p share)
               (mkdir-p inc)
               (install-file "user-scripts/error.parse" share)
               (copy-recursively "examples" (string-append share "examples"))
               (for-each (lambda (file) (install-file file inc))
                         (find-files "include" "\\.h"))))))))
    (inputs
     (list audiofile
           libxi
           libxmu
           mesa
           freeglut
           flex
           bison
           sed
           grep))
    (home-page "https://taopm.sourceforge.net/")
    (synopsis "Sound Synthesis with Physical Models")
    (description "Tao is a software package for sound synthesis using physical
models.  It provides a virtual acoustic material constructed from masses and
springs which can be used as the basis for building quite complex virtual
musical instruments.  Tao comes with a synthesis language for creating and
playing instruments and a C++ API for those who would like to use it as an
object library.")
    (license license:gpl2+)))

(define-public tao-synth
  (let ((commit "f3aedd81efbc775574e591081b57ae1c08427064")
        (revision "1"))
    (package
      (name "tao-synth")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lucasw/tao_synth")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jds2l3cb96b02jxd7lmrjjl9s7mylnrvg6fpw0j8c141bk8vyg3"))))
      (build-system cmake-build-system)
      (arguments (list #:tests? #false))  ;there are no tests
      (inputs
       (list glfw freeglut))
      (native-inputs
       (list gcc-7))
      (home-page "https://github.com/lucasw/tao_synth")
      (synopsis "Sound synthesis with physical models")
      (description "Tao is a software package for sound synthesis using physical
models.  It provides a virtual acoustic material constructed from masses and
springs which can be used as the basis for building quite complex virtual
musical instruments.  Tao comes with a synthesis language for creating and
playing instruments and a C++ API for those who would like to use it as an
object library.")
      (license license:lgpl2.0+))))

(define-public csound
  (package
    (name "csound")
    (version "6.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csound/csound")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgasyk8j4cl9178vci1dph63nks3cgwhf8y1d04z9dc8gg15dyn"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bison flex gettext-minimal zlib))
    (inputs
     (list alsa-lib
           boost
           jack-1
           ladspa
           liblo
           libsndfile
           pulseaudio))
    (home-page "https://csound.com/")
    (synopsis "Sound and music computing system")
    (description
     "Csound is a user-programmable and user-extensible sound processing
language and software synthesizer.")
    (license license:lgpl2.1+)))

(define-public midicomp
  ;; The latest tagged release is 9 years old and there have been
  ;; unreleased fixes, so we take the last commit.
  (let ((commit "70f76963cb0cdb3cbe03ec6e7246b1fb885d3c68")
        (revision "1"))
    (package
      (name "midicomp")
      (version (git-version "0.0.8" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/markc/midicomp")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "12zh247c6v88ssy4l8v7yirh4bl2jcc1ch7f4gdry79a82kai1gf"))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f))  ; no "check" target
      (synopsis "Convert SMF MIDI files to and from plain text")
      (description
       "midicomp can manipulate SMF (Standard MIDI File) files.  It can both
  read and write SMF files in 0 or format 1 and also read and write its own
  plain text format.  This means a SMF file can be turned into easily
  parseable text, edited with any text editor or filtered through any script
  language, and recompiled back into a binary SMF file.")
      (home-page "https://github.com/markc/midicomp")
      (license license:agpl3))))

(define-public mt32emu
  (package
    (name "mt32emu")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/munt/munt")
             (commit
              (string-append "libmt32emu_"
                             (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06d3jzx69nwy9jj6jv9q6rhq5399mp51w6d5mijg3fmwr4al13fd"))))
    (build-system cmake-build-system)
    (arguments (list
                #:tests? #f             ;no tests.
                #:configure-flags #~(list "-Dmunt_WITH_MT32EMU_SMF2WAV=FALSE"
                                          "-Dmunt_WITH_MT32EMU_QT=FALSE")))
    (home-page "https://sourceforge.net/projects/munt/")
    (synopsis "Pre-GM Roland MIDI device emulator")
    (description
     "libmt32emu is a C/C++ library which approximately emulates
the Roland MT-32, CM-32L and LAPC-I synthesizer modules. It is part of the
Munt project.")
    (license license:gpl2+)))

(define-public clalsadrv
  (package
    (name "clalsadrv")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/clalsadrv-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0bsacx3l9065gk8g4137qmz2ij7s9x06aldvacinzlcslw7bd1kq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "libs/Makefile"
               (("/sbin/ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "libs")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libclalsadrv.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libclalsadrv.so.2"))
             #t))
         ;; no configure script
         (delete 'configure))))
    (inputs
     (list alsa-lib fftw))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "C++ wrapper around the ALSA API")
    (description
     "clalsadrv is a C++ wrapper around the ALSA API simplifying access to
ALSA PCM devices.")
    (license license:gpl2+)))

(define-public amb-plugins
  (package
    (name "amb-plugins")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/AMB-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0x4blm4visjqj0ndqr0cg776v3b7lvplpc8cgi9n51llhavn0jpl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory-and-tool-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/lib/ladspa")
                (string-append (assoc-ref outputs "out") "/lib/ladspa"))
               (("/usr/bin/install") (which "install"))
               (("/bin/rm") "#"))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA ambisonics plugins")
    (description
     "The AMB plugins are a set of LADSPA ambisonics plugins, mainly to be
used within Ardour.  Features include: mono and stereo to B-format panning,
horizontal rotator, square, hexagon and cube decoders.")
    (license license:gpl2+)))

(define-public mcp-plugins
  (package
    (name "mcp-plugins")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/MCP-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "06a9r1l85jmg7l1cvc3788mk8ra0xagjfy1rmhw3b80y4n0vlnvc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "Chorus, phaser, and vintage high-pass and low-pass filters")
    (description
     "This package provides various LADSPA plugins.  @code{cs_chorus} and
@code{cs_phaser} provide chorus and phaser effects, respectively;
@code{mvclpf24} provides four implementations of the low-pass filter used in
vintage Moog synthesizers; @code{mvchpf24} is based on the voltage-controlled
high-pass filter by Robert Moog.  The filters attempt to accurately emulate
the non-linear circuit elements of their original analog counterparts.")
    (license license:gpl2+)))

(define-public rev-plugins
  (package
    (name "rev-plugins")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/REV-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1ikpinxm00pkfi259bnkzhsy3miagrjgdihaaf5x4v7zac29j3g7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA reverb plugin")
    (description
     "This package provides a stereo reverb LADSPA plugin based on the
well-known greverb.")
    (license license:gpl2+)))

(define-public fil-plugins
  (package
    (name "fil-plugins")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/FIL-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1scfv9j7jrp50r565haa4rvxn1vk2ss86xssl5qgcr8r45qz42qw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA four-band parametric equalizer plugin")
    (description
     "This package provides a LADSPA plugin for a four-band parametric
equalizer.  Each section has an active/bypass switch, frequency, bandwidth and
gain controls.  There is also a global bypass switch and gain control.

The 2nd order resonant filters are implemented using a Mitra-Regalia style
lattice filter, which is stable even while parameters are being changed.

All switches and controls are internally smoothed, so they can be used @code{live}
without any clicks or zipper noises.  This makes this plugin suitable for use
in systems that allow automation of plugin control ports, such as Ardour, or
for stage use.")
    (license license:gpl2+)))

(define-public ste-plugins
  (package
    (name "ste-plugins")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/STE-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0s3c9w5xihs87cnd1lh9xgj3maabjdyh6bl766qp5lhkg3ax8zy6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA stereo width plugin")
    (description
     "This package provides a LADSPA plugin to manipulate the stereo width of
audio signals.")
    (license license:gpl2+)))

(define-public vco-plugins
  (package
    (name "vco-plugins")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/VCO-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1xzqdg3b07r7zww05y9bb737l9dxvfkv28m3fyak1aazaci3rsgl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out"))
               (("/bin/cp") (which "cp")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA plugin for synthesizer oscillators")
    (description
     "The @code{blvco} LADSPA plugin provides three anti-aliased oscillators:

@enumerate
@item Pulse-VCO, a dirac pulse oscillator with flat amplitude spectrum
@item Saw-VCO, a sawtooth oscillator with 1/F amplitude spectrum
@item Rec-VCO, a square / rectangle oscillator
@end enumerate\n

All oscillators are low-pass filtered to provide waveforms similar to the
output of analog synthesizers such as the Moog Voyager.")
    (license license:gpl2+)))

(define-public wah-plugins
  (package
    (name "wah-plugins")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/WAH-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1wkbjarxdhjixkh7d5abralj11dj2xxg644fz3ycd7qyfgfvjfgd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA Autowah effect plugin")
    (description
     "This package provides a LADSPA plugin for a Wah effect with envelope
follower.")
    (license license:gpl2+)))

(define-public g2reverb
  (package
    (name "g2reverb")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/g2reverb-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "18wb8vj1kky5glr76s34awbi8qzplsmf3wjbd7a12hfv4j0bkwrj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA stereo reverb plugin")
    (description
     "This package provides a LADSPA plugin for a stereo reverb effect.")
    (license license:gpl2+)))

(define-public fluidsynth
  (package
    (name "fluidsynth")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FluidSynth/fluidsynth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05lr9f0q4x1kvgfa3xrfmagpwvijv9m1s316aa9figqlkcc5vv4k"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libdir
           (lambda _
             ;; Install libraries to /lib, not /lib64.
             (substitute* "CMakeLists.txt"
               (("LIB_SUFFIX \\$\\{_init_lib_suffix\\}")
                "LIB_SUFFIX \"\"")))))))
    (inputs
     (list ladspa))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In Libs.private of fluidsynth.pc.
     (list alsa-lib
           glib
           jack-1
           libsndfile
           readline))
    (home-page "https://www.fluidsynth.org/")
    (synopsis "SoundFont synthesizer")
    (description
     "FluidSynth is a real-time software synthesizer based on the SoundFont 2
specifications.  FluidSynth reads and handles MIDI events from the MIDI input
device.  It is the software analogue of a MIDI synthesizer.  FluidSynth can
also play midifiles using a Soundfont.")
    (license license:lgpl2.1+)))

(define-public faad2
  (package
    (name "faad2")
    (version "2.8.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/faac/faad2-src/faad2-"
                       (version-major+minor version) ".0/"
                       "faad2-" version ".tar.gz"))
       (sha256
        (base32 "0va284hndhn0ynm4lyn219qw4y8wa4agfkqgwlaji7bqp6nkyp4q"))))
    (build-system gnu-build-system)
    (home-page "https://www.audiocoding.com/faad2.html")
    (synopsis "MPEG-4 and MPEG-2 AAC decoder")
    (description
     "FAAD2 is an MPEG-4 and MPEG-2 AAC decoder supporting LC, Main, LTP, SBR, -PS, and DAB+.")
    (license license:gpl2+)))

(define-public faust
  (package
    (name "faust")
    (version "0.9.90")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grame-cncm/faust")
                    (commit (string-append "v"
                                           (string-map (lambda (c)
                                                         (if (char=? c #\.) #\- c))
                                                       version)))))
              (file-name (string-append "faust-" version "-checkout"))
              (sha256
               (base32
                "0qc6iwjd3i80jdyjc186c6ywipmjzl8wlsp4050pbr56q4rlkd4z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; no "configure" script
         (delete 'configure)
         ;; Files appear under $out/share/faust that are read-only.  The
         ;; install phase tries to overwrite them and fails, so we change
         ;; the permissions first.
         (add-before 'install 'fix-permissions
           (lambda _
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files "architecture/max-msp" ".*"))
             #t)))))
    (native-inputs
     (list unzip))
    (home-page "https://faust.grame.fr/")
    (synopsis "Signal processing language")
    (description
     "Faust is a programming language for realtime audio signal processing.")
    (license license:gpl2+)))

(define-public faust-2
  (package
    (inherit faust)
    (version "2.75.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/grame-cncm/faust/"
                                  "releases/download/" version
                                  "/faust-" version ".tar.gz"))
              (sha256
               (base32
                "11ww02zmj3vnva1w52hs9wkxvhwwf53agklyzm2c7gysw0jfvkw9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; There are tests, but they are unit/regression tests scattered in 17
      ;; different test directories, and little information indicating whether
      ;; they are worth running for Guix.  Ignore tests for now.
      #:tests? #f
      #:configure-flags
      #~(list "-C" "backends/all.cmake"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          ;; The upstream package uses make to run cmake during the build stage.
          ;; Here we ignore the Makefile and call cmake directly.
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (chdir "build")
              (apply invoke "cmake" configure-flags)))
          ;; The sound2faust tool would be built in the Makefile's "world" target
          (add-after 'install 'sound2faust
            (lambda _
              (chdir "../tools/sound2faust")
              (setenv "PREFIX" #$output)
              (invoke "make")
              (invoke "make" "install"))))))
    (native-inputs
     (list llvm-18 pkg-config which))
    (inputs
     (list libsndfile libmicrohttpd ncurses openssl zlib))))

(define-public freepats
  (package
    (name "freepats")
    (version "20060219")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://freepats.zenvoid.org/freepats-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "12iw36rd94zirll96cd5k0va7p5hxmf2shvjlhzihcmjaw8flq82"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (string-append %output "/share/freepats")))
                     (setenv "PATH" (string-append
                                     (assoc-ref %build-inputs "bzip2") "/bin:"
                                     (assoc-ref %build-inputs "tar") "/bin"))
                     (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                     (chdir "freepats")
                     ;; Use absolute pattern references
                     (substitute* "freepats.cfg"
                       (("Tone_000") (string-append out "/Tone_000"))
                       (("Drum_000") (string-append out "/Drum_000")))
                     (mkdir-p out)
                     (copy-recursively "." out)
                     #t))))
    (native-inputs
     (list tar bzip2))
    (home-page "https://freepats.zenvoid.org")
    (synopsis "GUS compatible patches for MIDI players")
    (description
     "FreePats is a project to create a free and open set of GUS compatible
patches that can be used with softsynths such as Timidity and WildMidi.")
    ;; GPLv2+ with exception for compositions using these patches.
    (license license:gpl2+)))

(define-public freepats-gm
  (package
    (name "freepats-gm")
    (version "20210329")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freepats.zenvoid.org/SoundSets/"
                                  "FreePats-GeneralMIDI/FreePatsGM-SF2-"  version ".7z"))
              (sha256
               (base32
                "19a1mp9yi33j2zff4mjvhrjz97dwwgjwzfdlf84j9xyydhx0crhc"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("p7zip" ,p7zip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((dir (string-append "FreePatsGM-SF2-" ,version))
             (file (string-append "FreePatsGM-" ,version ".sf2"))
             (out (string-append %output "/share/soundfonts"))
             (doc (string-append %output "/share/doc/freepats-gm-" ,version)))
         (use-modules (guix build utils))
         (invoke (string-append (assoc-ref %build-inputs "p7zip") "/bin/7z")
                 "e" (assoc-ref %build-inputs "source")
                 (string-append dir "/" file)
                 (string-append dir "/gpl.txt")
                 (string-append dir "/cc0.txt")
                 (string-append dir "/readme.txt"))
         (mkdir-p out)
         (copy-file file (string-append out "/FreePatsGM.sf2"))
         (mkdir-p doc)
         (for-each
          (lambda (file)
            (copy-file file (string-append doc "/" file)))
          (find-files "." "\\.txt$"))
         #t)))
    (home-page "https://freepats.zenvoid.org/SoundSets/general-midi.html")
    (synopsis "General MIDI sound set")
    (description "FreePats is a project to create a free (as in free software)
collection of digital instruments for music production.  This sound bank is a
partial release of the General MIDI sound set.")
    (license (list
              license:gpl3+ ; with sampling exception
              license:cc0))))

(define-public guitarix
  (package
    (name "guitarix")
    (version "0.44.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/guitarix/guitarix/guitarix2-"
                   version ".tar.xz"))
             (sha256
              (base32
               "063cdvrzrpnj08dm22z651hy5cz5bnsywf1z8b2kib2i9xskvs3p"))))
    (build-system waf-build-system)
    (arguments
     (list #:tests? #f ; no "check" target
           #:configure-flags
           #~(list
              ;; Add the output lib directory to the RUNPATH.
              (string-append "--ldflags=-Wl,-rpath=" #$output "/lib"))
           #:phases
           '(modify-phases %standard-phases
              (add-after 'unpack 'python3.11-compatibility
                (lambda _
                  (substitute* "wscript"
                    (("'rU'") "'r'")))))))
    (inputs
     (list libsndfile
           boost
           curl
           avahi
           eigen
           lv2
           lilv
           ladspa
           jack-1
           gtkmm-3
           gtk+
           fftwf
           lrdf
           zita-resampler
           zita-convolver))
    (native-inputs
     (list gperf
           faust
           intltool
           gettext-minimal
           pkg-config
           sassc))
    (home-page "https://guitarix.org/")
    (synopsis "Virtual guitar amplifier")
    (description "Guitarix is a virtual guitar amplifier running JACK.
Guitarix takes the signal from your guitar as a mono-signal from your sound
card.  The input is processed by a main amp and a rack-section.  Both can be
routed separately and deliver a processed stereo-signal via JACK.  You may
fill the rack with effects from more than 25 built-in modules including stuff
from a simple noise gate to modulation effects like flanger, phaser or
auto-wah.")
    (license license:gpl2+)))

(define-public guitarix-lv2
  (package (inherit guitarix)
    (name "guitarix-lv2")
    (arguments
     (substitute-keyword-arguments (package-arguments guitarix)
       ((#:configure-flags flags)
        `(cons "--no-standalone" ,flags))))))

(define-public rakarrack
  (package
    (name "rakarrack")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rakarrack/rakarrack/"
                                  "rakarrack-" version "/rakarrack-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1rpf63pdn54c4yg13k7cb1w1c7zsvl97c4qxcpz41c8l91xd55kn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* '("src/process.C"
                                 "src/global.h")
                    (("#include <Fl/") "#include <FL/"))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-utils
           fltk-1.3
           libx11
           libxext
           libxfixes
           libxft
           libxrender
           libxpm
           fontconfig
           freetype
           jack-1
           alsa-lib
           libsndfile
           libsamplerate
           zlib))
    (home-page "https://rakarrack.sourceforge.net/")
    (synopsis "Audio effects processor")
    (description
     "Rakarrack is a richly featured multi-effects processor emulating a
guitar effects pedalboard.  Effects include compressor, expander, noise gate,
equalizers, exciter, flangers, chorus, various delay and reverb effects,
distortion modules and many more.  Most of the effects engine is built from
modules found in the excellent software synthesizer ZynAddSubFX.  Presets and
user interface are optimized for guitar, but Rakarrack processes signals in
stereo while it does not apply internal band-limiting filtering, and thus is
well suited to all musical instruments and vocals.")
    ;; The code is explicitly licensed under the GPL version 2 only.
    (license license:gpl2)))

(define-public ir
  (package
    (name "ir")
    (version "1.3.4")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tomszilagyi/ir.lv2")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0svmjhg4r6wy5ci5rwz43ybll7yxjv7nnj7nyqscbzhr3gi5aib0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                              ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "INSTDIR="
                            (assoc-ref %outputs "out") "/lib/lv2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)        ; no configure script
         ;; See https://github.com/tomszilagyi/ir.lv2/pull/20
         (add-after 'unpack 'fix-type
           (lambda _
             (substitute* '("ir_gui.cc" "lv2_ui.h")
               (("_LV2UI_Descriptor") "LV2UI_Descriptor"))
             #t)))))
    (inputs
     (list libsndfile
           libsamplerate
           lv2
           glib
           gtk+-2
           zita-convolver))
    (native-inputs
     (list pkg-config))
    (home-page "https://tomszilagyi.github.io/plugins/ir.lv2")
    (synopsis "LV2 convolution reverb")
    (description
     "IR is a low-latency, real-time, high performance signal convolver
especially for creating reverb effects.  It supports impulse responses with 1,
2 or 4 channels, in any soundfile format supported by libsndfile.")
    (license license:gpl2+)))

;; Packages depending on JACK should always prefer jack-2.
;; JACK1 is provided for legacy applications
(define-public jack-1
  (package
    (name "jack")
    (version "0.125.0")
    (source
     (origin
       (method url-fetch)
       ;; jackaudio.org/downloads/jack-audio-connection-kit-0.125.0.tar.gz
       ;; no longer exists (404).  Use an unofficial mirror.
       (uri (string-append "https://crux.ster.zone/downloads/"
                           "jack-audio-connection-kit/"
                           "jack-audio-connection-kit-" version ".tar.gz"))
       (sha256
        (base32 "0i6l25dmfk2ji2lrakqq9icnwjxklgcjzzk65dmsff91z2zva5rm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-configure
                    (lambda _
                      (substitute* "configure"
                        ;; Install to <out/lib> regardless of platform.
                        (("libnn=lib64") "libnn=lib"))
                      #t)))))
    (inputs
     (list alsa-lib readline))
    ;; uuid.h is included in the JACK type headers
    ;; db.h is included in the libjack metadata headers
    (propagated-inputs
     `(("libuuid" ,util-linux "lib")
       ("bdb" ,bdb)))
    (native-inputs
     (list pkg-config))
    (home-page "https://jackaudio.org/")
    (synopsis "JACK audio connection kit")
    (description
     "JACK is a low-latency audio server.  It can connect a number of
different applications to an audio device, as well as allowing them to share
audio between themselves.  JACK is different from other audio server efforts
in that it has been designed from the ground up to be suitable for
professional audio work.  This means that it focuses on two key areas:
synchronous execution of all clients, and low latency operation.")
    ;; Most files are licensed under the GPL. However, the libjack/ tree is
    ;; licensed under the LGPL in order to allow for proprietary usage.
    (license (list license:gpl2+ license:lgpl2.1+))))

;; Packages depending on JACK should always prefer jack-2.  Both jack-1 and
;; jack-2 implement the same API.
(define-public jack-2
  (package
    (inherit jack-1)
    (name "jack2")
    (version "1.9.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jackaudio/jack2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sbrffmdbajvrk7iqvsvrnwnpvmicvbjyq3f52r6ashdsznsz03b"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags '("--dbus" "--alsa")
       #:phases
       (modify-phases %standard-phases
         ;; Python 3.11 has removed the 'U' (universal newline) mode.  It has
         ;; been the default since Python 3.3.
         (add-after 'unpack 'python-compatibility
           (lambda _
             (substitute* '("waflib/Context.py"
                            "waflib/ConfigSet.py")
               (("m='rU'") "m='r'")
               (("read\\('rU'") "read('r'"))))
         (add-before 'configure 'set-linkflags
           (lambda _
             ;; Ensure -lstdc++ is the tail of LDFLAGS or the simdtests.cpp
             ;; will not link with undefined reference to symbol
             ;; '__gxx_personality_v0@@CXXABI_1.3'
             (setenv "LDFLAGS" "-lstdc++")
             ;; Add $libdir to the RUNPATH of all the binaries.
             (substitute* "wscript"
               ((".*CFLAGS.*-Wall.*" m)
                (string-append m
                               "    conf.env.append_unique('LINKFLAGS',"
                               "'-Wl,-rpath=" %output "/lib')\n")))))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure 'jack_control' runs with the correct PYTHONPATH.
             (wrap-program (search-input-file outputs "bin/jack_control")
               `("GUIX_PYTHONPATH" ":"
                 prefix (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs
     (list alsa-lib
           bash-minimal
           dbus
           expat
           libsamplerate
           opus
           python-dbus
           readline))
    (native-inputs
     (list pkg-config))
    ;; Most files are under GPLv2+, but some headers are under LGPLv2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jack-example-tools
  (package
    (name "jack-example-tools")
    (version "3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jackaudio/jack-example-tools")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x684clxqib1bq3zvvrqlh7hb3arb1bf672xyx1jbwv76dcmm5mh"))))
    (build-system meson-build-system)
    (inputs
     (list alsa-lib
           jack-2
           libsndfile
           opus
           readline))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/jackaudio/jack-example-tools")
    (synopsis "Tools for JACK connections")
    (description "This package provides tools for managing JACK connections
and testing or configuring the JACK session.  Tools include @code{jack_lsp},
@code{jack_connect}, and @code{jack_transport}.")
    ;; Most files are under GPLv2+, but zalsa is GPLv3+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public jacktrip
  (package
    (name "jacktrip")
    (version "1.6.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jacktrip/jacktrip/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0719ng799kingv0y9yk07bvnmprk25c09ph3yaia5dhapg0jz17m"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out"))
                     "-config" "novs"
                     "-config" "noupdater"
                     "jacktrip.pro"))))))
    (inputs
     (list jack-2
           python
           python-jinja2
           python-pyyaml
           qtbase-5
           rtaudio))
    (native-inputs
     (list pkg-config qtbase-5)) ;for qmake
    (home-page "https://jacktrip.github.io/jacktrip/")
    (synopsis "Multi-machine audio system for network music performance")
    (description
     "JackTrip is a multi-machine audio system used for network music
performance over the Internet.  It supports any number of channels (as many as
the computer/network can handle) of bidirectional, high quality, uncompressed
audio signal streaming.")
    (license (list license:gpl3+ license:lgpl3 license:expat))))

(define-public jack-mixer
  (package
    (name "jack-mixer")
    (version "19")
    (source
     (origin
       (file-name (git-file-name name version))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jack-mixer/jack_mixer")
             (commit (string-append "release-" version))))
       (sha256
        (base32 "18m6a9asbwaslw418i2w04kgc6jgdpw01i3kawdqy903kw66hnhj"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'path-patch
                     (lambda _
                       (substitute* '("meson.build")
                         (("'/', 'etc', 'xdg'")
                          (string-append "'"
                                         #$output "'")))))
                   (add-after 'install 'wrap-path
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (bin (string-append out "/bin/"))
                              (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                              (version #$(version-major+minor (package-version
                                                               (this-package-input
                                                                "python"))))
                              (lib (string-append out "/lib/python" version
                                                  "/site-packages")))
                         (wrap-program (string-append bin "jack_mixer")
                           `("GUIX_PYTHONPATH" ":" prefix
                             (,(getenv "GUIX_PYTHONPATH") ,lib))
                           `("GI_TYPELIB_PATH" ":" prefix
                             (,gi-typelib-path)))))))))
    (native-inputs (list pkg-config python-cython python-docutils
                         gettext-minimal glib))
    (inputs (list bash-minimal))
    (propagated-inputs (list gtk+
                             `(,gtk+ "bin")
                             python
                             python-wrapper
                             python-pygobject
                             python-pycairo
                             python-platformdirs
                             jack-2))
    (synopsis
     "JACK Mixer: A multi-channel audio mixer for the JACK Audio Connection Kit")
    (description
     "The jack_mixer is a GTK+ JACK audio mixer app with a look & handling
similar to hardware mixing desks.  It has lot of useful features, apart
from being able to mix multiple JACK audio streams.")
    (home-page "https://rdio.space/jackmixer/")
    (license license:gpl2+)))

(define-public jalv
  (package
    (name "jalv")
    (version "1.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/jalv-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1q8mzjv577vdi64s47gd4pg0ydzxvs32cwrb1d64v90f52qpgbpd"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'build-PIC
           ;; The default -fPIE #errors when combined with our Qt packages.
           ;; Work around the broken meson.build script clobbering c_args.
           (lambda _
             (substitute* "meson.build"
               (("'-DZIX_STATIC'" match)
                (string-append match ", '-fPIC'"))))))))
    (inputs
     (list lv2
           lilv
           suil
           gtk+
           qtbase-5
           jack-1))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/jalv.html")
    (synopsis "Simple LV2 host for JACK")
    (description
     "Jalv is a simple but fully featured LV2 host for JACK.  It runs LV2
plugins and exposes their ports as JACK ports, essentially making any LV2
plugin function as a JACK application.")
    (license license:isc)))

(define-public ladspa
  (package
    (name "ladspa")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       ;; Since the official link is dead,
       ;; we download the tarball from Debian or Internet Archive.
       (uri (list (string-append "http://http.debian.net"
                                 "/debian/pool/main/l/ladspa-sdk/ladspa-sdk_"
                                 version ".orig.tar.gz")
                  (string-append "https://web.archive.org/web/20140717172251/"
                                 "http://www.ladspa.org/download/ladspa_sdk_"
                                 version ".tgz")))
       (sha256
        (base32
         "0srh5n2l63354bc0srcrv58rzjkn4gv8qjqzg8dnq3rs4m7kzvdm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; the "test" target is a listening test only
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (chdir "src")
             (let ((out (assoc-ref outputs "out")))
               (substitute* "makefile"
                 (("/usr/lib/ladspa/") (string-append out "/lib/ladspa/"))
                 (("/usr/include/")    (string-append out "/include/"))
                 (("/usr/bin/")        (string-append out "/bin/"))
                 (("-mkdirhier")       "mkdir -p")
                 (("^CC.*")            "CC = gcc\n")
                 (("^CPP.*")           "CPP = g++\n")))
             #t))
         (delete 'build))))
    (native-search-paths
     (list (search-path-specification
            (variable "LADSPA_PATH")
            (files '("lib/ladspa")))))
    ;; Since the home page is gone, we provide a link to the archived version.
    (home-page
     "https://web.archive.org/web/20140729190945/http://www.ladspa.org/")
    (synopsis "Linux Audio Developer's Simple Plugin API (LADSPA)")
    (description
     "LADSPA is a standard that allows software audio processors and effects
to be plugged into a wide range of audio synthesis and recording packages.")
    (license license:lgpl2.1+)))

(define-public libbs2b
  (package
    (name "libbs2b")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bs2b/libbs2b/" version
                                  "/libbs2b-" version ".tar.lzma"))
              (sha256
               (base32
                "1mcc4gjkmphczjybnsrip3gq1f974knzys7x49bv197xk3fn8wdr"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libsndfile))
    (home-page "https://sourceforge.net/projects/bs2b/")
    (synopsis "Bauer stereophonic-to-binaural DSP")
    (description
     "The Bauer stereophonic-to-binaural DSP (bs2b) library and plugins is
designed to improve headphone listening of stereo audio records.  Recommended
for headphone prolonged listening to disable superstereo fatigue without
essential distortions.")
    (license license:expat)))

(define-public ladspa-bs2b
  (package
    (name "ladspa-bs2b")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bs2b/plugins/LADSPA%20plugin/"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b4aipbj1ba5k99gbc7gmgy14sywyrjd8rpyqj5l905j0mjv8jg2"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list ladspa libbs2b))
    (home-page "https://sourceforge.net/projects/bs2b/")
    (synopsis "Bauer stereophonic-to-binaural DSP - LADSPA plugin")
    (description "The Bauer stereophonic-to-binaural DSP (bs2b) library and
plugins is designed to improve headphone listening of stereo audio records.
Recommended for headphone prolonged listening to disable superstereo fatigue
without essential distortions.  This package contains a LADSPA plugin for use
with applications that support them (e.g. PulseAudio).")
    (license license:gpl2+)))

(define-public liblo
  (package
    (name "liblo")
    (version "0.31")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/liblo/liblo/" version
                                 "/liblo-" version ".tar.gz"))
             (sha256
              (base32
               "0l67rkdhfa8cffa0nynql3lh2xlbn1454h6qxhjddp1029p48krb"))))
    (build-system gnu-build-system)
    (arguments
     `(;; liblo test FAILED
       ;; liblo server error 19 in setsockopt(IP_ADD_MEMBERSHIP): No such device
       #:tests? #f))
    (home-page "https://liblo.sourceforge.net")
    (synopsis "Implementation of the Open Sound Control protocol")
    (description
     "liblo is a lightweight library that provides an easy to use
implementation of the Open Sound Control (@dfn{OSC}) protocol.")
    (license license:lgpl2.1+)))

(define-public rtaudio
  (package
    (name "rtaudio")
    (version "5.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thestk/rtaudio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "189xphhf0winf8b60dx1kk2biz811wk6ps44br7l1lyfhymxcjmi"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib jack-1 pulseaudio))
    (synopsis "Common API for real-time audio I/O")
    (description
     "RtAudio is a set of C++ classes that provides a common API for real-time
audio input/output.  It was designed with the following objectives:

@itemize
@item object-oriented C++ design
@item simple, common API across all supported platforms
@item only one source and one header file for easy inclusion in programming
projects
@item allow simultaneous multi-api support
@item support dynamic connection of devices
@item provide extensive audio device parameter control
@item allow audio device capability probing
@item automatic internal conversion for data format, channel number
compensation, (de)interleaving, and byte-swapping
@end itemize")
    (home-page "https://www.music.mcgill.ca/~gary/rtaudio/")
    ;; License is expat with a non-binding request to send modifications to
    ;; original developer.
    (license license:expat)))

(define-public python-pyaudio
  (package
    (name "python-pyaudio")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyAudio" version))
       (sha256
        (base32 "17pvc27pn2xbisbq7nibhidyw8h2kyms7g2xbyx7nlxwfbdzbpam"))))
    (build-system python-build-system)
    (inputs
     (list portaudio))
    (home-page "https://people.csail.mit.edu/hubert/pyaudio/")
    (synopsis "Bindings for PortAudio v19")
    (description "This package provides bindings for PortAudio v19, the
cross-platform audio input/output stream library.")
    (license license:expat)))

(define-public python-pulsectl
  (package
    (name "python-pulsectl")
    (version "24.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pulsectl" version))
              (sha256
               (base32
                "0r9igs365cqgrn1m55a8qjz0hc446nwjm3p3i9kphbj5gl7dazk9"))))
    (build-system python-build-system)
    (inputs (list pulseaudio))
    (arguments
     `(#:tests? #f                      ; tests try to communicate with PulseAudio
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "pulsectl/_pulsectl.py"
               (("libpulse.so.0")
                (string-append (search-input-file inputs "/lib/libpulse.so.0")))))))))
    (home-page "https://github.com/mk-fg/python-pulse-control")
    (synopsis
     "Python bindings for mixer-like controls in PulseAudio")
    (description
     "This package provides a Python high-level interface and ctypes-based
bindings for PulseAudio (libpulse), to use in simple synchronous code.
This wrapper is mostly for mixer-like controls and introspection-related
operations, as opposed to e.g. submitting sound samples to play and
player-like clients.")
    (license license:expat)))

(define-public python-pyliblo
  (package
    (name "python-pyliblo")
    (version "0.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://das.nasophon.de/download/pyliblo-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "13vry6xhxm7adnbyj28w1kpwrh0kf7nw83cz1yq74wl21faz2rzw"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (native-inputs
     (list python-cython))
    (inputs
     (list liblo))
    (home-page "http://das.nasophon.de/pyliblo/")
    (synopsis "Python bindings for liblo")
    (description
     "Pyliblo is a Python wrapper for the liblo Open Sound Control (OSC)
library.  It supports almost the complete functionality of liblo, allowing you
to send and receive OSC messages using a nice and simple Python API.  Also
included are the command line utilities @code{send_osc} and @code{dump_osc}.")
    (license license:lgpl2.1+)))

(define-public python-pyliblo3
  (package
    (name "python-pyliblo3")
    (version "0.16.3")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/gesellkammer/pyliblo3")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1rr2m8jxa5yxyb3pw6h93kvdxg7x0m6sxxxvgn34vq8k8mg1kz21"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-cython python-setuptools python-wheel))
    (inputs (list liblo))
    (home-page "https://github.com/gesellkammer/pyliblo3")
    (synopsis "Python bindings for liblo")
    (description
     "Pyliblo is a Python wrapper for the liblo Open Sound Control (OSC)
library.  It supports almost the complete functionality of liblo, allowing you
to send and receive OSC messages using a nice and simple Python API.  Also
included are the command line utilities @code{send_osc} and @code{dump_osc}.")
    (license license:lgpl2.1+)))

(define-public python-soundfile
  (package
    (name "python-soundfile")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "soundfile" version))
       (sha256
        (base32
         "0mc3g5l9fzj57m62zrwwz0w86cbihpna3mikgh8kpmz7ppc9jcz8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Error opening 'tests/stereo.mp3': File contains data in an
      ;; unimplemented format.
      '(list "-k" "not test_write_mp3_compression")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "soundfile.py"
               (("_find_library\\('sndfile'\\)")
                (string-append "\"" (search-input-file inputs "/lib/libsndfile.so")
                               "\""))))))))
    (propagated-inputs
     (list python-cffi python-numpy libsndfile))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/bastibe/SoundFile")
    (synopsis "Python bindings for libsndfile")
    (description "This package provides python bindings for libsndfile based on
CFFI and NumPy.")
    (license license:expat)))

(define-public python-soxr
  (package
    (name "python-soxr")
    (version "0.5.0.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "soxr" version))
       (sha256
        (base32 "0wzz7j0z814mm99xr19vfrwp2x904lbwhf513x7085m4x3rvk4kh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'find-nanobind
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((python #$(this-package-native-input "python"))
                     (version (python-version python))
                     (nanobind (search-input-file
                                inputs
                                (string-append "lib/python" version
                                               "/site-packages/nanobind/"
                                               "cmake/nanobind-config.cmake"))))
                (setenv "CMAKE_PREFIX_PATH"
                        (string-append (dirname nanobind)
                                       ":" (getenv "CMAKE_PREFIX_PATH")))))))))
    (propagated-inputs (list python-numpy))
    (native-inputs (list cmake-minimal
                         python
                         python-linkify-it-py
                         python-myst-parser
                         python-nanobind
                         python-pytest
                         python-scikit-build-core
                         python-setuptools
                         python-setuptools-scm
                         python-sphinx
                         python-typing-extensions
                         python-wheel))
    (home-page "https://github.com/dofuuz/python-soxr")
    (synopsis "High quality, one-dimensional sample-rate conversion library")
    (description
     "Python-SoXR is a Python wrapper of libsoxr, a high quality,
one-dimensional sample-rate conversion library.")
    (license license:lgpl2.1)))

(define-public python-python3-midi
  (package
    (name "python-python3-midi")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python3_midi" version))
       (sha256
        (base32
         "1z9j1w7mpn3xhkcpxmqm5rvmj6nffb5rf14bv7n3sdh07nf6n7sf"))))
    (build-system python-build-system)
    (home-page "https://github.com/NFJones/python3-midi")
    (synopsis "Python MIDI API")
    (description "This package provides a python API to read and write MIDI
files.")
    (license license:expat)))

(define-public audio-to-midi
  (package
    (name "audio-to-midi")
    (version "2020.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/NFJones/audio-to-midi")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "12wf17abn3psbsg2r2lk0xdnk8n5cd5rrvjlpxjnjfhd09n7qqgm"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-cffi
            python-cython
            python-numpy
            python-progressbar2
            python-pycparser
            python-python3-midi
            python-soundfile))
    (native-inputs
     (list libsndfile))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versions
           (lambda _
             (substitute* "requirements.txt" (("==") ">=")))))))
    (home-page "https://github.com/NFJones/audio-to-midi")
    (synopsis "Convert audio to multichannel MIDI")
    (description "@command{audio-to-midi} converts audio files to multichannel
MIDI files.  It accomplishes this by performing FFTs on all channels of the
audio data at user-specified time steps.  It then separates the resulting
frequency analysis into equivalence classes which correspond to the twelve tone
scale; the volume of each class being the average volume of its constituent
frequencies.  This data is then formatted to MIDI and written to disk.")
    (license license:expat)))

(define-public lilv
  (package
    (name "lilv")
    (version "0.24.22")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/lilv-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wwzkz91zv0cj8dkr7aqsryznihhbkhwaplv81ik7j4zwp84kybn"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'full-store-path-to-shared-library
            (lambda _
              (with-directory-excursion "bindings/python"
                (substitute* "lilv.py"
                  (("liblilv-0.so") (string-append #$output "/lib/liblilv-0.so")))))))))
    ;; Required by lilv-0.pc.
    (propagated-inputs
     (list lv2 serd sord sratom))
    (native-inputs
     (list python pkg-config))
    (home-page "https://drobilla.net/software/lilv.html")
    (synopsis "Library to simplify use of LV2 plugins in applications")
    (description
     "Lilv is a C library to make the use of LV2 plugins as simple as possible
for applications.  Lilv is the successor to SLV2, rewritten to be
significantly faster and have minimal dependencies.")
    (license license:isc)))

(define-public lv2
  (package
    (name "lv2")
    (version "1.18.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://lv2plug.in/spec/lv2-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0gwm63jrvg9lww0rl3sjkgbjwfz0vascpb19cfxmhkmm477ipibq"))))
    (build-system meson-build-system)
    (inputs
     ;; Leaving off cairo and gtk+-2.0 which are needed for example plugins
     (list libsndfile))
    (native-inputs
     (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "https://lv2plug.in/")
    (synopsis "LV2 audio plugin specification")
    (description
     "LV2 is an open specification for audio plugins and host applications.
At its core, LV2 is a simple stable interface, accompanied by extensions which
add functionality to support the needs of increasingly powerful audio
software.")
    (license license:isc)))

(define-public ttl2c
  (package
    (name "ttl2c")
    (version "1.0.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/lvtk/ttl2c")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0aybx8i5i0sridi9130a3937xgmfmjkk8m48f9whvhlhbzwy3xbl"))))
    (build-system waf-build-system)
    (arguments
     (list
      #:tests? #false  ;no check target
      #:phases
      `(modify-phases %standard-phases
         (add-before 'configure 'setup-waf
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((waf (assoc-ref inputs "python-waf")))
               (copy-file (string-append waf "/bin/waf") "waf")))))))
    (inputs (list boost))
    (native-inputs (list python-waf))
    (home-page "https://github.com/lvtk/ttl2c")
    (synopsis "Turtle to C header conversion utility for LV2 plugins")
    (description
     "This package provides a conversion utility for LV2 Plugin developers to
generate C headers from Turtle files.")
    (license license:gpl3+)))

(define-public lv2-mda-piano
  (package
    (name "lv2-mda-piano")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://git.elephly.net/software/lv2-mdametapiano.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07lywf6lpfpndg3i9w752mmlg2hgn1bwp23h8b0mdj6awh67abqd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list
                     "TYPE=mdaPiano"
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no check target
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     (list lv2 lvtk))
    (native-inputs
     (list pkg-config ttl2c))
    (home-page "https://elephly.net/lv2/mdapiano.html")
    (synopsis "LV2 port of the mda Piano plugin")
    (description "An LV2 port of the mda Piano VSTi.")
    (license license:gpl3+)))

(define-public lv2-mda-epiano
  (package (inherit lv2-mda-piano)
    (name "lv2-mda-epiano")
    (arguments
     `(#:make-flags (list
                     "TYPE=mdaEPiano"
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no check target
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://elephly.net/lv2/mdaepiano.html")
    (synopsis "LV2 port of the mda EPiano plugin")
    (description "An LV2 port of the mda EPiano VSTi.")))

(define-public lvtk-2
  ;; Use the latest commit, as the latest release was made in 2014 and depends
  ;; on Python 2.
  (let ((commit "a73feabe772f9650aa071e6a4df660e549ab7c48")
        (revision "0"))
    (package
      (name "lvtk")
      (version (git-version "2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lvtk/lvtk")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0scmv8b4jlm88d21dqqchjy98wb93zclc9x960h213gdi871vsaj"))))
      (build-system waf-build-system)
      (arguments (list #:tests? #f))    ;no check target
      (inputs (list boost gtkmm lv2))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/lvtk/lvtk")
      (synopsis "C++ libraries for LV2 plugins")
      (description
       "The LV2 Toolkit (LVTK) contains libraries that wrap the LV2 C API and
extensions into easy to use C++ classes.  It is the successor of
lv2-c++-tools.")
      (license license:isc))))

(define-public lvtk-1
  ;; Use the latest commit, as the latest release was made in 2014 and depends
  ;; on Python 2.
  (let ((commit "23dd99531d88d7821b69f6f0d60516ef322a6729")
        (revision "0"))
    (package
      (name "lvtk")
      (version (git-version "1.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lvtk/lvtk")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iw7skvsn3whw69dhcxbbdns7mssaf6z6iyzxjav53607ibyfr8d"))))
      (build-system waf-build-system)
      (arguments
       (list
        #:tests? #false                 ;no check target
        #:configure-flags
        #~(list (string-append "--boost-includes="
                               #$(this-package-input "boost")
                               "/include"))
        #:phases
        `(modify-phases %standard-phases
           (add-before 'configure 'setup-waf
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((waf (assoc-ref inputs "python-waf")))
                 (copy-file (string-append waf "/bin/waf") "waf")))))))
      (inputs (list boost gtkmm-2 lv2))
      (native-inputs (list pkg-config python-waf))
      (home-page "https://github.com/lvtk/lvtk")
      (synopsis "C++ libraries for LV2 plugins")
      (description
       "The LV2 Toolkit (LVTK) contains libraries that wrap the LV2 C API and
extensions into easy to use C++ classes.  It is the successor of
lv2-c++-tools.")
      (license license:isc))))

(define-public lvtk lvtk-1)

(define-public openal
  (package
    (name "openal")
    (version "1.23.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://openal-soft.org/openal-releases/openal-soft-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "08avhhfd96x4c18p8ys3va85nhx31xgpa3bz1ckmfkjc2f4lnvvr"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-full-library-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "alc/backends/pulseaudio.cpp"
                (("#define PALIB \"libpulse\\.so\\.0\"")
                 (string-append "#define PALIB \""
                                (search-input-file inputs "lib/libpulse.so.0")
                                "\"")))
              (substitute* "alc/backends/alsa.cpp"
                (("LoadLib\\(\"libasound\\.so\\.2\"\\)")
                 (string-append "LoadLib(\""
                                (search-input-file inputs "lib/libasound.so.2")
                                "/lib/libasound.so.2"
                                "\")"))))))))
    (inputs (list alsa-lib pulseaudio))
    (synopsis "3D audio API")
    (description
     "OpenAL provides capabilities for playing audio in a virtual 3D
environment.  Distance attenuation, doppler shift, and directional sound
emitters are among the features handled by the API.  More advanced effects,
including air absorption, occlusion, and environmental reverb, are available
through the EFX extension.  It also facilitates streaming audio, multi-channel
buffers, and audio capture.")
    (home-page "https://openal-soft.org/")
    (properties
     '((upstream-name . "openal-soft")))
    (license license:lgpl2.0+)))

(define-public freealut
  (package
    (name "freealut")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              ;; Upstream url is unclear, many systems use Fedora, there is also
              ;; https://github.com/vancegroup/freealut though the status of it
              ;; (official? unofficial?) is not clear.
              (uri (string-append
                    "https://pkgs.fedoraproject.org/repo/pkgs/" name "/" name "-"
                    version ".tar.gz" "/e089b28a0267faabdb6c079ee173664a/" name
                    "-" version ".tar.gz"))
              (sha256
               (base32
                "0kzlil6112x2429nw6mycmif8y6bxr2cwjcvp18vh6s7g63ymlb0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))  ; no check target
    (inputs
     (list openal))
    (synopsis "Free implementation of OpenAL's ALUT standard")
    (description "freealut is the OpenAL Utility Toolkit.")
    (home-page "https://kcat.strangesoft.net/openal.html")
    (license license:lgpl2.0)))

(define-public alure
  (package
    (name "alure")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://kcat.tomasu.net/alure-releases/"
                                  "alure-" version ".tar.bz2"))
              (sha256
               (base32
                "0w8gsyqki21s1qb2s5ac1kj08i6nc937c0rr08xbw9w9wvd6lpj6"))
              (patches (search-patches "alure-dumb-2.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:configure-flags '("-DMODPLUG=ON")))
    (native-inputs (list pkg-config))
    (inputs (list dumb
                  flac
                  mpg123
                  libmodplug
                  libsndfile
                  libvorbis
                  openal))
    (home-page "https://kcat.tomasu.net/alure.html")
    (synopsis "OpenAL utility library")
    (description
     "ALURE is a utility library to help manage common tasks with OpenAL applications.
This includes device enumeration and initialization, file loading, and
streaming.")
    (license license:expat)))

(define-public pa-notify
  (package
    (name "pa-notify")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ikrivosheev/pa-notify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04wq0bdnb3r27l5wlf8c1ijq18ffywqmdv584l6hbi3i5k0sm7nz"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ;no check target
    (inputs (list glib
                  libnotify
                  pulseaudio))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/ikrivosheev/pa-notify")
    (synopsis "PulseAudio or PipeWire volume notification")
    (description "The pa-notify daemon sends notifications about
the current volume level of PulseAudio or PipeWire using libnotify.")
    (license license:expat)))

(define-public patchage
  (package
    (name "patchage")
    (version "1.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/patchage-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1m472rkvv7kr57xnvmvds3iq3fj129mbw878427djc21rfg2lq80"))))
    (build-system meson-build-system)
    (arguments `(#:tests? #f))                    ;no check target
    (inputs
     (list alsa-lib
           jack-2
           ganv
           glibmm
           gtkmm-2
           dbus-glib))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/patchage.html")
    (synopsis "Modular patch bay for audio and MIDI systems")
    (description
     "Patchage is a modular patch bay for audio and MIDI systems based on JACK
and ALSA.")
    (license license:gpl3+)))

(define-public pcaudiolib
  (package
    (name "pcaudiolib")
    (version "1.1")
    (home-page "https://github.com/espeak-ng/pcaudiolib")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c55hlqqh0m7bcb3nlgv1s4a22s5bgczr1cakjh3767rjb10khi0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     (list autoconf automake libtool pkg-config which))
    (inputs
     (list alsa-lib pulseaudio))
    (synopsis "Portable C audio library")
    (description
     "The Portable C Audio Library (pcaudiolib) provides a C@tie{}API to
different audio devices such as ALSA or PulseAudio.")
    (license (list license:gpl3+
                   ;; The bundled TPCircularBuffer uses a custom license.
                   (license:non-copyleft
                    "file://src/TPCircularBuffer/README.markdown")))))

(define-public qjackctl
  (package
    (name "qjackctl")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qjackctl/qjackctl/"
                                  version "/qjackctl-" version ".tar.gz"))
              (sha256
               (base32
                "0wzimnxb9yjj155l0hqb57smf0158a4bbzi6bj11pp60njld4zqn"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (inputs
     (list alsa-lib
           jack-1
           portaudio
           qtbase
           qtsvg))
    (native-inputs
     (list pkg-config qttools))
    (home-page "https://qjackctl.sourceforge.io/")
    (synopsis "Jack server control application")
    (description "Control a Jack server.  Allows you to plug various sources
into various outputs and to start, stop and configure jackd")
    (license license:gpl2+)))

(define-public qjackrcd
  (package
    (name "qjackrcd")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/orouits/qjackrcd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l5iq2mkqd4gn9yr8xbh9qlpp1clcflazychl4vhdbz0bzq4c6al"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out"))))))))
    (native-inputs
     (list qtbase-5)) ; for qmake
    (inputs
     (list jack-1 libsndfile qtbase-5))
    (home-page "https://sourceforge.net/projects/qjackrcd/")
    (synopsis "Stereo audio recorder for JACK")
    (description "QJackRcd is a simple graphical stereo recorder for JACK
supporting silence processing for automatic pause, file splitting, and
background file post-processing.")
    (license license:gpl2+)))

(define-public supercollider
  (package
    (name "supercollider")
    (version "3.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/supercollider/supercollider")
             (commit (string-append "Version-" version))
             ;; for nova-simd, nova-tt, hidapi, TLSF, oscpack
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dkpnaly4m2j41ypy7xj5m2yhbl4ykw3vbnam345z4dk6qhyj9b1"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        ;; The build system doesn't allow us to unbundle the following
        ;; libraries.  hidapi is also heavily patched and upstream not
        ;; actively maintained.
        #~(let ((keep-dirs '("nova-simd" "nova-tt" "hidapi"
                             "TLSF-2.4.6" "oscpack_1_1_0" "." "..")))
            (with-directory-excursion "./external_libraries"
              (for-each
               delete-file-recursively
               (scandir "."
                        (lambda (x)
                          (and (eq? (stat:type (stat x)) 'directory)
                               (not (member (basename x) keep-dirs)))))))
            ;; To find the Guix provided ableton-link library.
            (substitute* "lang/CMakeLists.txt"
              (("include\\(\\.\\./external_libraries/link/\
AbletonLinkConfig\\.cmake\\)")
               "find_package(AbletonLink NAMES AbletonLink ableton-link \
link REQUIRED)"))))))
    (build-system cmake-build-system)
    (outputs
     '("out"                            ;core language
       "ide"))                          ;qt ide
    (arguments
     (list
      #:configure-flags
      #~(list "-DSYSTEM_BOOST=ON"
              "-DSYSTEM_YAMLCPP=ON"
              "-DSC_QT=ON"
              "-DCMAKE_BUILD_TYPE=Release"
              "-DFORTIFY=ON"
              "-DLIBSCSYNTH=ON"
              "-DSC_EL=OFF")      ;scel is packaged individually as emacs-scel
      #:phases
      #~(modify-phases %standard-phases
          ;; HOME must be defined otherwise supercollider throws a "ERROR:
          ;; Primitive '_FileMkDir' failed." error when generating the doc.
          ;; The graphical tests also hang without it.
          (add-after 'unpack 'set-home-directory
            (lambda _
              (setenv "HOME" (getcwd))))
          (add-after 'unpack 'patch-scclass-dir
            (lambda _
              (let* ((scclass-dir
                      (string-append #$output
                                     "/share/SuperCollider/SCClassLibrary")))
                (substitute* "lang/LangSource/SC_LanguageConfig.cpp"
                  (((string-append
                     "SC_Filesystem::instance\\(\\)\\.getDirectory"
                     "\\(DirName::Resource\\) / CLASS_LIB_DIR_NAME"))
                   (string-append "Path(\"" scclass-dir "\")"))))))
          (add-after 'patch-scclass-dir 'fix-struct-SOUNDFILE-tag
            (lambda _
              (display (getcwd)) (newline)
              (substitute* "include/plugin_interface/SC_SndBuf.h"
                (("SNDFILE_tag")
                 "sf_private_tag"))))
          (add-before 'build 'prepare-x
            (lambda _
              (system "Xvfb &")
              (setenv "DISPLAY" ":0")))
          (add-before 'install 'install-ide
            (lambda _
              (let* ((ide #$output:ide)
                     (scide "editors/sc-ide/scide"))
                (install-file scide
                              (string-append ide "/bin"))
                (delete-file scide)))))))
    (native-inputs
     (list ableton-link pkg-config qttools-5 xorg-server-for-tests))
    (inputs (list jack-1
                  libsndfile
                  fftw
                  libxt
                  readline              ;readline support for sclang's CLI
                  alsa-lib              ;for sclang's MIDI interface
                  eudev                 ;for user interactions with devices
                  avahi                 ;zeroconf service discovery support
                  icu4c
                  boost
                  boost-sync
                  yaml-cpp
                  python-wrapper        ;there were warnings in the build process
                  ruby                  ;there were warnings in the build process
                  qtbase-5
                  qtdeclarative-5
                  qtsvg-5
                  qtwebchannel-5
                  qtwebsockets-5))
    (propagated-inputs                  ;to get native-search-path
     (list qtwebengine-5))
    (home-page "https://github.com/supercollider/supercollider")
    (synopsis "Synthesis engine and programming language")
    (description "SuperCollider is a synthesis engine (@code{scsynth} or
@code{supernova}) and programming language (@code{sclang}).  It can be used
for experimenting with sound synthesis and algorithmic composition.

SuperCollider requires jackd to be installed in your user profile and your
user must be allowed to access the realtime features of the kernel.  Search
for \"realtime\" in the index of the Guix manual to learn how to achieve this
using Guix System.")
    (license license:gpl2+)))

(define-public libshout-idjc
  (package
    (name "libshout-idjc")
    (version "2.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libshoutidjc.idjc.p"
                           "/libshout-idjc-" version ".tar.gz"))
       (sha256
        (base32 "1cgbym1qms408l4anc0imlcf091yk9kic4s9n7zcri3xzbi8lv1z"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libshout))
    (propagated-inputs
     ;; In Requires.private of shout-idjc.pc.
     (list libogg libtheora libvorbis speex))
    (home-page "https://idjc.sourceforge.io/")
    (synopsis "Broadcast streaming library with IDJC extensions")
    (description "This package provides libshout plus IDJC extensions.")
    ;; GNU Library (not Lesser) General Public License.
    (license license:lgpl2.0+)))

(define-public redumper
  (package
    (name "redumper")
    (version "561")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/superg/redumper")
                    (commit (string-append "build_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r0wfi0fn3rq7s28p89rkgpgf567akd8z25l8r9sj7p4p3xp9m91"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:cmake cmake-next
      #:build-type "Release"
      ;; The build system uses CMake modules features that are only available
      ;; when using Ninja.
      #:configure-flags #~(list "-GNinja"
                                "-DREDUMPER_CLANG_USE_LIBCPP=ON"
                                (string-append "-DREDUMPER_VERSION_BUILD="
                                               #$version)
                                "-DCMAKE_BUILD_TYPE=Release")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            (lambda _
              ;; The CMAKE_SYSTEM_VERSION is undefined when cross-compiling.
              (substitute* "CMakeLists.txt"
                (("\\$\\{CMAKE_SYSTEM_VERSION}")
                 "\"${CMAKE_SYSTEM_VERSION}\""))))
          (add-after 'unpack 'adjust-CPLUS_INCLUDE_PATH
            ;; The libcxx include/c++/v1 directory is not exposed via
            ;; CPLUS_INCLUDE_PATH by default, causing errors like
            ;; "fatal error: 'format' file not found".
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append
                       (search-input-directory inputs
                                               "/include/c++/v1")
                       ":" (getenv "CPLUS_INCLUDE_PATH")))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "cmake" "--build" "."
                      "-j" (number->string
                            (if parallel-build?
                                (parallel-job-count)
                                1)))))
          (replace 'check
            (lambda* (#:key build-type parallel-tests? tests?
                      #:allow-other-keys)
              (when tests?
                (invoke "ctest" "-C" build-type
                        "-j" (number->string
                              (if parallel-tests?
                                  (parallel-job-count)
                                  1))))))
          (replace 'install
            (lambda _
              ;; There is no CMake install target; manually install the
              ;; binary.
              (install-file "redumper"
                            (string-append #$output "/bin")))))))
    ;; As of GCC 14, the C++ modules feature is not complete enough, hence the
    ;; use of Clang.
    (native-inputs (list ninja clang-toolchain-19))
    (inputs (list libcxx))
    (home-page "https://github.com/superg/redumper")
    (synopsis "Low-level CD/DVD dumper")
    (description "@command{redumper} is a low-level byte perfect CD disc
dumper.  It supports incremental dumps, advanced SCSI/C2 repair, intelligent
audio CD offset detection, among other features.  @command{redumper} is also a
general purpose DVD/HD-DVD/Blu-ray disc dumper.")
    (license license:gpl3+)))

(define-public resample
  (package
    (name "resample")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ccrma.stanford.edu/~jos/gz/resample-"
                                  version
                                  ".tar.gz"))
              (sha256 (base32
                       "074zj8ydp05yy1hjcglfv3hkvj4cm50f9nralka1992pm6yf8yvy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (synopsis "Sampling rate conversion and filter design utilities")
    (description "This package contains the @command{resample} and
@command{windowfilter} command line utilities.  The @command{resample} command
allows changing the sampling rate of a sound file, while the
@command{windowfilter} command allows designing Finite Impulse Response (FIR)
filters using the so-called @emph{window method}.")
    (home-page "https://ccrma.stanford.edu/~jos/resample/Free_Resampling_Software.html")
    (license license:lgpl2.1+)))

(define-public rubberband
  (package
    (name "rubberband")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://breakfastquay.com/files/releases/"
                              "rubberband-" version ".tar.bz2"))
              (sha256
               (base32
                "1s98h0pzxlffha52paniysm7dch5rrflw1ifbfriig33xq9h61dg"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "-Dresampler=libsamplerate"
             "-Dfft=fftw"                  ;To avoid using bundled version
             "-Ddefault_library=shared"))) ;Don't build static library
    (inputs
     (list ladspa lv2 vamp))
    (propagated-inputs
     (list fftw libsamplerate)) ;required by rubberband.pc
    (native-inputs
     (list pkg-config))
    (home-page "https://breakfastquay.com/rubberband/")
    (synopsis "Audio time-stretching and pitch-shifting library")
    (description
     "Rubber Band is a library and utility program that permits changing the
tempo and pitch of an audio recording independently of one another.")
    (license license:gpl2+)))

(define-public rtmidi
  (package
    (name "rtmidi")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.music.mcgill.ca/~gary/rtmidi"
                                  "/release/rtmidi-" version ".tar.gz"))
              (file-name (string-append "rtmidi-" version ".tar.gz"))
              (sha256
               (base32
                "1ff2yfq3k4l209fr71v3w98fpjjv1chs09vkbmxj03lcikahxns8"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib jack-2))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.music.mcgill.ca/~gary/rtmidi")
    (synopsis "Cross-platform MIDI library for C++")
    (description
     "RtMidi is a set of C++ classes (RtMidiIn, RtMidiOut, and API specific
classes) that provide a common cross-platform API for realtime MIDI
input/output.")
    (license license:expat)))

(define-public rtmidi-4.0
  (package
    (inherit rtmidi)
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.music.mcgill.ca/~gary/rtmidi"
                                  "/release/rtmidi-" version ".tar.gz"))
              (file-name (string-append "rtmidi-" version ".tar.gz"))
              (sha256
               (base32
                "1k962ljpnwyjw9jjiky2372khhri1wqvrj5qsalfpys31xqzw31p"))))))

(define-public sratom
  (package
    (name "sratom")
    (version "0.6.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/sratom-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1i12wcnv52r05zr5wjmpvbsvbjbm3hkv8frral2kvrc326cmghbi"))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; In Requires of sratom-0.pc.
     (list lv2 serd sord))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/sratom.html")
    (synopsis "Library for serialising LV2 atoms to/from RDF")
    (description
     "Sratom is a library for serialising LV2 atoms to/from RDF, particularly
the Turtle syntax.")
    (license license:isc)))

(define-public suil
  (package
    (name "suil")
    (version "0.10.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/suil-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0klkxzzx77bg0jwv3a7sn1ar30333y0bjg8b83zifpixwz9kwjik"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f))                    ;no check target
    (inputs
     (list lv2
           gtk+-2
           gtk+
           qtbase-5
           qtx11extras))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/suil/")
    (synopsis "Library for loading and wrapping LV2 plugin UIs")
    (description
     "Suil is a lightweight C library for loading and wrapping LV2 plugin UIs.

Suil makes it possible to load a UI of a toolkit in a host using another
toolkit.  The API is designed such that hosts do not need to explicitly
support specific toolkits – if Suil supports a particular toolkit, then UIs in
that toolkit will work in all hosts that use Suil automatically.

Suil currently supports every combination of Gtk, Qt, and X11.")
    (license license:isc)))

(define-public libebur128
  (package
    (name "libebur128")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jiixyj/libebur128")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkpz5rzz1j1srhprbh89669gw8z5f1njpvcnxqgf7qax69vd8sh"))))
    (build-system cmake-build-system)
    (arguments
     `(;; Tests require proprietary .wav files. See
       ;; https://github.com/jiixyj/libebur128/issues/82.
       #:tests? #f
       #:configure-flags '("-DBUILD_STATIC_LIBS=OFF")))
    (home-page "https://github.com/jiixyj/libebur128")
    (synopsis "Library implementing the EBU R 128 loudness standard")
    (description
     "@code{libebur128} is a C library that implements the EBU R 128 standard
for loudness normalisation.")
    (license license:expat)))

(define-public timidity++
  (package
    (name "timidity++")
    (version "2.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/timidity/TiMidity++"
                                  "/TiMidity++-" version
                                  "/TiMidity++-" version ".tar.bz2"))
              (sha256
               (base32
                "1xf8n6dqzvi6nr2asags12ijbj1lwk1hgl3s27vm2szib8ww07qn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-audio=alsa,flac,jack,ao,vorbis,speex"
             "--enable-ncurses"
             "--enable-server"
             "--enable-alsaseq"
             (string-append "--with-default-path="
                            (assoc-ref %outputs "out") "/etc/timidity"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-config
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/etc/timidity")))
               (mkdir-p out)
               (call-with-output-file
                   (string-append out "/timidity.cfg")
                 (lambda (port)
                   (format port (string-append "source "
                                               (assoc-ref %build-inputs "freepats")
                                               "/share/freepats/freepats.cfg")))))
             #t)))))
    (inputs
     (list alsa-lib
           ao
           flac
           jack-1
           libogg
           libvorbis
           speex
           ncurses
           freepats))
    (native-inputs
     (list pkg-config))
    (home-page "https://timidity.sourceforge.net/")
    (synopsis "Software synthesizer for playing MIDI files")
    (description
     "TiMidity++ is a software synthesizer.  It can play MIDI files by
converting them into PCM waveform data; give it a MIDI data along with digital
instrument data files, then it synthesizes them in real-time, and plays.  It
can not only play sounds, but also can save the generated waveforms into hard
disks as various audio file formats.")
    (license license:gpl2+)))

(define-public vamp
  (package
    (name "vamp")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://code.soundsoftware.ac.uk/attachments/download/"
                    "2691" ; This mysterious number changes with each update
                    "/vamp-plugin-sdk-" version ".tar.gz"))
              (sha256
               (base32
                "0pzpkxrz71fzqd2m83kjyafzqzrifzsq5phcn7mqq52blii3gbxf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-libvamp-hostsdk.la
           (lambda* (#:key outputs #:allow-other-keys)
             ;; https://bugs.launchpad.net/ubuntu/+source/vamp-plugin-sdk/+bug/1253656
             (for-each delete-file
                       (let ((out (assoc-ref outputs "out")))
                         (list (string-append out "/lib/libvamp-sdk.la")
                               (string-append out "/lib/libvamp-hostsdk.la"))))
             #t)))))
    (inputs
     (list libsndfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://vamp-plugins.org")
    (synopsis "Modular and extensible audio processing system")
    (description
     "Vamp is an audio processing plugin system for plugins that extract
descriptive information from audio data — typically referred to as audio
analysis plugins or audio feature extraction plugins.")
    (license
     (license:x11-style
      "https://code.soundsoftware.ac.uk/projects/vamp-plugin-sdk/repository/entry/COPYING"))))

(define-public libsbsms
  (package
    (name "libsbsms")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbsms/sbsms/" version
                           "/libsbsms-" version ".tar.gz"))
       (sha256
        (base32 "1vmf84iy4dkwxv887grnlsfk43fmhd9gbg26gc2kgcv40sbkvayf"))))
    (build-system gnu-build-system)
    (native-inputs (list automake))
    (arguments
     `(#:configure-flags
       ;; Disable the use of SSE unless on x86_64.
       ,(if (not (string-prefix? "x86_64" (or (%current-target-system)
                                              (%current-system))))
            ''("--disable-sse")
            ''())
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-ar-lib-path
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Originally a symlink to '/usr/local/share/automake-1.12/ar-lib'.
            (delete-file "ar-lib")
            (symlink
             (search-input-file inputs
                                (string-append "/share/automake-"
                                               ,(version-major+minor
                                                 (package-version automake))
                                               "/ar-lib"))
             "ar-lib")
            #t)))))
    (home-page "https://sbsms.sourceforge.net/")
    (synopsis "Library for time stretching and pitch scaling of audio")
    (description
     "SBSMS (Subband Sinusoidal Modeling Synthesis) is software for time
stretching and pitch scaling of audio.  This package contains the library.")
    ;; There is no explicit declaration of a license, but a COPYING file
    ;; containing gpl2.
    (license license:gpl2)))

(define-public stargate-sbsms
  ;; Stargate's fork of sbsms.
  (let ((commit "90fab3440063dc9b6c1c2a8f74c2d92bd0e423f9")
        (revision "0"))
    (package/inherit libsbsms
      (name "stargate-sbsms")
      (version (git-version "0" revision commit))
      (home-page "https://github.com/stargatedaw/stargate-sbsms")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11srnzgpavcj6n70zjdm7488jzrprk71mg9dgr1sa6vwp575hf2m"))))
      (arguments
       (substitute-keyword-arguments (package-arguments libsbsms)
         ((#:phases phases)
          #~(modify-phases #$phases
              (delete 'fix-ar-lib-path)
              (add-before 'build 'change-directory
                (lambda _
                  (chdir "cli")))
              (replace 'configure
                (lambda _
                  (setenv "DESTDIR" #$output)
                  (setenv "PREFIX" "/")))
              (add-after 'install 'rename-sbsms
                (lambda _
                  (with-directory-excursion (string-append #$output
                                                           "/bin")
                    (rename-file "sbsms" "stargate-sbsms"))))
              (delete 'check)))))
      (native-inputs
       (list libsndfile))
      (properties '((hidden? . #t))))))

(define-public libkeyfinder
  (package
    (name "libkeyfinder")
    (version "2.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mixxxdj/libkeyfinder")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nvhdzy0m3bchk3dpnspv2f518p2v9fjcrv36z1sva1pv9a2g35w"))))
    (build-system cmake-build-system)
    (native-inputs
     (list catch2))
    (inputs
     (list fftw))
    (home-page "https://mixxxdj.github.io/libkeyfinder/")
    (synopsis "Musical key detection for digital audio")
    (description
     "@code{libkeyfinder} is a small C++11 library for estimating the musical
key of digital audio.")
    (license license:gpl3+)))

(define-public wavpack
  (package
    (name "wavpack")
    (version "5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dbry/WavPack/releases/download/"
                           version "/wavpack-" version ".tar.xz"))
       (sha256
        (base32 "0ycbqarw25x7208jilh86vwwiqklr7f617jps9mllqc659mnmpjb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-static"
             "--enable-tests")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./cli/wvtest" "--default" "--short"))
             #t)))))
    (home-page "https://www.wavpack.com/")
    (synopsis "Hybrid lossless audio codec")
    (description
     "WavPack is an audio compression format with lossless, lossy and hybrid
compression modes.  This package contains command-line programs and library to
encode and decode wavpack files.")
    (license license:bsd-3)))

(define-public libmixed
  ;; Release is much outdated.
  (let ((commit "9b2668e0d85175b0e92864cfbf1b9e58f77c92e0")
        (revision "1"))
    (package
      (name "libmixed")
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shirakumo/libmixed")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ql2h0hh4jl96sc9i6mk1d6qq261bvsfapinvzr9gx3lpzycpfb7"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DBUILD_STATIC=OFF"
                "-DCMAKE_CXX_FLAGS=-O3 -fPIC"
                "-DCMAKE_C_FLAGS=-O3 -fPIC")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-paths
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("/usr/local") #$output))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "./tester")))))))
      (native-inputs (list doxygen graphviz))
      (inputs (list mpg123 ncurses))
      (home-page "https://github.com/Shirakumo/libmixed")
      (synopsis "Low-level audio mixer pipeline library")
      (description
       "Libmixed is a library for real-time audio processing pipelines for use
in audio/video/games.  It can serve as a base architecture for complex DSP
systems.")
      (license (list license:bsd-2 ; libsamplerate
                     license:gpl2 ; spiralfft
                     license:zlib)))))

(define-public libmodplug
  (package
    (name "libmodplug")
    (version "0.8.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/modplug-xmms/"
                    name "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pnri98a603xk47smnxr551svbmgbzcw018mq1k6srbrq6kaaz25"))))
    (build-system gnu-build-system)
    (home-page "https://modplug-xmms.sourceforge.net/")
    (synopsis "Mod file playing library")
    (description
     "Libmodplug renders mod music files as raw audio data, for playing or
conversion.  mod, .s3m, .it, .xm, and a number of lesser-known formats are
supported.  Optional features include high-quality resampling, bass expansion,
surround and reverb.")
    (license license:public-domain)))

(define-public libxmp
  (package
    (name "libxmp")
    (version "4.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/libxmp/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kycz4jsyvmf7ny9227b497wc7y5ligydi6fvvldmkf8hk63ad9m"))))
    (build-system gnu-build-system)
    (home-page "https://xmp.sourceforge.net/")
    (synopsis "Module player library")
    (description
     "Libxmp is a library that renders module files to PCM data.  It supports
over 90 mainstream and obscure module formats including Protracker (MOD),
Scream Tracker 3 (S3M), Fast Tracker II (XM), and Impulse Tracker (IT).")
    (license license:lgpl2.1+)))

(define-public xmp
  (package
    (name "xmp")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/xmp/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "17i8fc7x7yn3z1x963xp9iv108gxfakxmdgmpv3mlm438w3n3g8x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxmp pulseaudio))
    (home-page "https://xmp.sourceforge.net/")
    (synopsis "Extended module player")
    (description
     "Xmp is a portable module player that plays over 90 mainstream and
obscure module formats, including Protracker MOD, Fasttracker II XM, Scream
Tracker 3 S3M and Impulse Tracker IT files.")
    (license license:gpl2+)))

(define-public soundtouch
  (package
    (name "soundtouch")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/soundtouch/soundtouch.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10znckb8mrnmvwj7vq12732al873qhqw27fpb5f8r0bkjdpcj3vr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool file))
    (home-page "https://www.surina.net/soundtouch/")
    (synopsis
     "Audio processing library for changing tempo, pitch and playback rate")
    (description
     "SoundTouch is an audio processing library for changing the tempo, pitch
and playback rates of audio streams or audio files.  It is intended for
application developers writing sound processing tools that require tempo/pitch
control functionality, or just for playing around with the sound effects.")
    (license license:lgpl2.1+)))

(define-public soundtouch-1/integer-samples
  (package
    (inherit soundtouch)
    (name "soundtouch")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/soundtouch/soundtouch.git")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ir961w5gz86cm6yivr1ypi6n2y52vn319gy2gvdkkbbz5wyjkrq"))))
    (arguments
     ;; Dolphin expects the samples to be of the integer type.
     (list #:configure-flags #~(list "--enable-integer-samples")
           #:phases #~(modify-phases %standard-phases
                        (replace 'bootstrap
                          (lambda _
                            ;; Avoid the bootstrap script, which has a broken
                            ;; shebang.
                            (invoke "autoreconf" "-vif"))))))))

(define-public stargate-soundtouch
  ;; Stargate's fork of soundtouch.
  (let ((commit "464f474c0be5d7e0970909dd30593012e4621468")
        (revision "0"))
    (package/inherit soundtouch
      (name "stargate-soundtouch")
      (version (git-version "0" revision commit))
      (home-page "https://github.com/stargatedaw/stargate-soundtouch")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page) (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1aw2j1f10p8n4s197b1nd3g1rjvwbrrszc9gwsbwk01c6nb3nr9v"))))
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'install 'rename-soundstretch
                   (lambda _
                     (with-directory-excursion (string-append #$output
                                                              "/bin")
                       (rename-file "soundstretch"
                                    "stargate-soundstretch")))))))
      (properties '((hidden? . #t))))))

(define-public sox
  (package
    (name "sox")
    (version "14.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/sox/sox/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "170lx90r1nlnb2j6lg00524iwvqy72p48vii4xc5prrh8dnrb9l1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; The upstream asks to identify the distribution to diagnose SoX
       ;; bug reports.
       '("--with-distro=Guix System Distribution")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           ao
           flac
           lame
           libid3tag
           libltdl
           libmad
           libpng
           libvorbis
           pulseaudio))
    (home-page "https://sox.sourceforge.net")
    (synopsis "Sound processing utility")
    (description
     "SoX (Sound eXchange) is a command line utility that can convert
various formats of computer audio files to other formats.  It can also
apply various effects to these sound files, and, as an added bonus, SoX
can play and record audio files.")
    ;; sox.c is distributed under GPL, while the files that make up
    ;; libsox are licensed under LGPL.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public soxr
  (package
    (name "soxr")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/soxr/soxr-" version
                       "-Source.tar.xz"))
       (sha256
        (base32 "12aql6svkplxq5fjycar18863hcq84c5kx8g6f4rj0lcvigw24di"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ;no 'check' target
    (home-page "https://sourceforge.net/p/soxr/wiki/Home/")
    (synopsis "One-dimensional sample-rate conversion library")
    (description
     "The SoX Resampler library (libsoxr) performs one-dimensional sample-rate
conversion.  It may be used, for example, to resample PCM-encoded audio.")
    (license license:lgpl2.1+)))

(define-public twolame
  (package
    (name "twolame")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/twolame/twolame/" version
                           "/twolame-" version ".tar.gz"))
       (sha256
        (base32 "0zf8sxqb02w07ah55b1y7ab643zmpbhn62spa9pqra0rc17l4dfc"))))
    (build-system gnu-build-system)
    (inputs
     (list libsndfile))
    (native-inputs
     (list perl which))               ;used in tests/test.pl
    (home-page "https://www.twolame.org/")
    (synopsis "MPEG Audio Layer 2 (MP2) encoder")
    (description
     "TwoLAME is an optimised MPEG Audio Layer 2 (MP2) encoder based on
tooLAME by Mike Cheng, which in turn is based upon the ISO dist10 code and
portions of LAME.")
    (license license:lgpl2.1+)))

(define-public portaudio
  (package
    (name "portaudio")
    (version "190600.20161030")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.portaudio.com/archives/pa_stable_v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "04qmin6nj144b8qb9kkd9a52xfvm0qdgm8bg8jbl7s3frmyiv8pm"))
       (patches (search-patches "portaudio-audacity-compat.patch"))))
    (build-system gnu-build-system)
    (inputs
     ;; TODO: Add ASIHPI.
     (list alsa-lib jack-1))
    ;; Autoreconf is necessary because the audacity-compat patch modifies .in
    ;; files.
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (arguments
     '(#:tests? #f                      ;no 'check' target
       #:parallel-build? #f             ;fails on some systems
       #:configure-flags '("--with-pic"
                           "--enable-cxx"
                           ;; XXX: The following prevents a build error
                           ;; because of missing depcomp when C++ bindings are
                           ;; requested.
                           "--disable-dependency-tracking")
       #:phases
       (modify-phases %standard-phases
         ;; This is needed for linking the static libraries
         (add-after 'unpack 'build-only-position-independent-code
           (lambda _
             (substitute* "configure.in"
               (("AC_PROG_LIBTOOL" m)
                (string-append m "\nAM_PROG_AR\nLT_INIT([pic-only])")))
             (delete-file "configure")
             #t))
         ;; Some headers are not installed by default, but are needed by
         ;; packages like Kaldi.
         (add-after 'install 'install-missing-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/common/pa_ringbuffer.h"
                           (string-append (assoc-ref outputs "out") "/include"))
             #t)))))
    (home-page "http://www.portaudio.com/")
    (synopsis "Audio I/O library")
    (description
     "PortAudio is a portable C/C++ audio I/O library providing a simple API
to record and/or play sound using a callback function or a blocking read/write
interface.")
    (license license:expat)))

(define-public qsynth
  (package
    (name "qsynth")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (list
              (string-append "mirror://sourceforge/qsynth/qsynth/" version
                             "/qsynth-" version ".tar.gz")
              (string-append "mirror://sourceforge/qsynth/qsynth (attic)"
                             "/qsynth-" version ".tar.gz")))
       (sha256
        (base32 "1cjg25nva5ivahr0qqlvf6ybnpcx9jgrxbp4vgwkk64b4k9wnd4n"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no "check" phase
    (native-inputs
     (list qttools pkg-config))
    (inputs
     (list fluidsynth qtbase qtsvg qtwayland))
    (home-page "https://qsynth.sourceforge.io")
    (synopsis "Graphical user interface for FluidSynth")
    (description
     "Qsynth is a GUI front-end application for the FluidSynth SoundFont
synthesizer written in C++.")
    (license license:gpl2+)))

(define-public rsound
  (package
    (name "rsound")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Themaister/RSound")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gspmr3klwnq98h17p5hc6ifygya4p80g4g8r7a1qavm3mv19waf"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           jack-1
           ao
           libsamplerate
           openal
           portaudio
           pulseaudio))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))))))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://themaister.net/rsound.html")
    (synopsis "Networked audio system")
    (description
     "RSound allows you to send audio from an application and transfer it
directly to a different computer on your LAN network.  It is an audio daemon
with a much different focus than most other audio daemons.")
    (license license:gpl3+)))

(define-public xjackfreak
  (package
    (name "xjackfreak")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/johnhldavis/xjackfreak")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18c546qidbrj0f5wfiq5llii2192xpln0ab3r4vpr7i3wybxqjfz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "docdir=" (assoc-ref %outputs "out")
                            "/share/doc/xjackfreak"))))
    (inputs
     (list jack-1 libx11 libxt libxext))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/johnhldavis/xjackfreak")
    (synopsis "JACK audio frequency analyzer and display")
    (description
     "XJackFreak is an audio analysis and equalizing tool for the Jack Audio
Connection Kit.  It can display the FFT of any input, modify it and output the
result.")
    (license license:gpl3+)))

(define-public zita-convolver
  (package
    (name "zita-convolver")
    (version "4.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-convolver-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(begin
                  (substitute* "source/Makefile"
                    (("^CXXFLAGS \\+= -march=native") ""))
                  #t))
              (modules '((guix build utils)))
              (sha256
               (base32
                "0prji66p86z2bzminywkwchr5bfgxcg2i8y803pydd1hzf2198cs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SUFFIX="))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "source/Makefile"
               (("ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "source")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libzita-convolver.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libzita-convolver.so.4"))
             #t))
         ;; no configure script
         (delete 'configure))))
    (inputs (list fftwf))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "Fast, partitioned convolution engine library")
    (description
     "Zita convolver is a C++ library providing a real-time convolution
engine.")
    (license license:gpl3+)))

(define-public zita-resampler
  (package
    (name "zita-resampler")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-resampler-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(begin
                  (substitute* '("apps/Makefile" "source/Makefile")
                    (("^CXXFLAGS \\+= -march=native") ""))
                  #t))
              (modules '((guix build utils)))
              (sha256
               (base32
                "1my5k2dh2dkvjp6xjnf9qy6i7s28z13kw1n9pwa4a2cpwbzawfr3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SUFFIX="))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-makefile-and-enter-directory
          (lambda _
            (substitute* "source/Makefile"
              (("ldconfig") "true")
              (("^LIBDIR =.*") "LIBDIR = lib\n"))
            (chdir "source")
            #t))
         (add-after
          'install 'install-symlink
          (lambda _
            (symlink "libzita-resampler.so"
                     (string-append (assoc-ref %outputs "out")
                                    "/lib/libzita-resampler.so.1"))
            #t))
         ;; no configure script
         (delete 'configure))))
    (home-page "https://kokkinizita.linuxaudio.org/linuxaudio/zita-resampler/resampler.html")
    (synopsis "C++ library for resampling audio signals")
    (description
     "Libzita-resampler is a C++ library for resampling audio signals.  It is
designed to be used within a real-time processing context, to be fast, and to
provide high-quality sample rate conversion.")
    (license license:gpl3+)))

(define-public zita-alsa-pcmi
  (package
    (name "zita-alsa-pcmi")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://kokkinizita.linuxaudio.org"
                                  "/linuxaudio/downloads/zita-alsa-pcmi-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "12d7vdg74yh21w69qi0wg57iz4876j94qbiq09bvscih6xz9y78s"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no "check" target
           #:make-flags #~(list (string-append "PREFIX="
                                               (assoc-ref %outputs "out"))
                                (string-append "SUFFIX=")
                                (string-append "CXX="
                                               #$(cxx-for-target)))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-makefile-and-enter-directory
                          (lambda _
                            (substitute* "source/Makefile"
                              (("ldconfig")
                               "true")
                              (("^LIBDIR =.*")
                               "LIBDIR = lib\n")
                              (("CXXFLAGS \\+= -march=native")
                               ""))
                            (chdir "source")))
                        (add-after 'install 'install-symlink
                          (lambda _
                            (symlink "libzita-alsa-pcmi.so"
                                     (string-append (assoc-ref %outputs "out")
                                      "/lib/libzita-alsa-pcmi.so.0"))))
                        ;; no configure script
                        (delete 'configure))))
    (inputs (list alsa-lib fftw))
    (properties `((tunable? . #t)))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "C++ wrapper around the ALSA API")
    (description
     "Zita-alsa-pcmi is a C++ wrapper around the ALSA API.  It provides easy
access to ALSA PCM devices, taking care of the many functions required to
open, initialise and use a hw: device in mmap mode, and providing floating
point audio data.")
    (license license:gpl3+)))

(define-public cuetools
  (package
    (name "cuetools")
    (version "1.4.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/svend/cuetools")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "02ksv1ahf1v4cr2xbclsfv5x17m9ivzbssb5r8xjm97yh8a7spa3"))))
    (build-system gnu-build-system)
    ;; The source checkout is not bootstrapped.
    (native-inputs
     (list autoconf automake flex bison))
    (synopsis "Cue and toc file parsers and utilities")
    (description "Cuetools is a set of programs that are useful for manipulating
and using CUE sheet (cue) files and Table of Contents (toc) files.  CUE and TOC
files are a way to represent the layout of a data or audio CD in a
machine-readable ASCII format.")
    (home-page "https://github.com/svend/cuetools")
    (license license:gpl2+)))

(define-public mp3guessenc
  (package
    (name "mp3guessenc")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mp3guessenc/mp3guessenc-"
                           (version-major+minor version) "/mp3guessenc-"
                           version ".tar.gz"))
       (sha256
        (base32 "1fa3sbwwn4p2v1749lzy040bfy1xfd574mf2frwgg9ikgk3vlb3c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure phase
    (home-page "https://mp3guessenc.sourceforge.io")
    (synopsis "Analyze MPEG layer I/II/III files")
    (description "mp3guessenc is a command line utility that tries to detect the
encoder used for an MPEG Layer III (MP3) file, as well as scan any MPEG audio
file (any layer) and print a lot of useful information.")
    (license license:lgpl2.1+)))

(define-public shntool
  (package
    (name "shntool")
    (version "3.0.10")
    (source (origin
             (method url-fetch)
             (uri (list
                    (string-append "http://etree.org/shnutils/shntool/dist/src/"
                                   "shntool-" version ".tar.gz")
                    (string-append "mirror://debian/pool/main/s/shntool/shntool_"
                                   version ".orig.tar.gz")))
             (sha256
              (base32
               "00i1rbjaaws3drkhiczaign3lnbhr161b7rbnjr8z83w8yn2wc3l"))))
    (build-system gnu-build-system)
    (synopsis "WAVE audio data processing tool")
    (description "shntool is a multi-purpose WAVE data processing and reporting
utility.  File formats are abstracted from its core, so it can process any file
that contains WAVE data, compressed or not---provided there exists a format
module to handle that particular file type.  It can also generate CUE files, and
use them split WAVE data into multiple files.")
    (home-page "http://etree.org/shnutils/shntool/")
    ;; 'install-sh' bears the x11 license
    (license (list license:gpl2+ license:x11))))

(define-public dcadec
  (package
    (name "dcadec")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/foo86/dcadec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07nd0ajizrp1w02bsyfcv18431r8m8rq8gjfmz9wmckpg7cxj2hs"))))
    (build-system gnu-build-system)
    (arguments
     ;; Test files are missing: https://github.com/foo86/dcadec/issues/53
     `(#:tests? #f
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; Build shared library.
             "CONFIG_SHARED=1"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Set proper runpath.
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; No configure script, just a hand-written Makefile.
         (delete 'configure))))
    (synopsis "DTS Coherent Acoustics decoder")
    (description "Dcadec is a DTS Coherent Acoustics surround sound decoder
with support for HD extensions.")
    (home-page "https://github.com/foo86/dcadec")
    (license license:lgpl2.1+)))

(define-public drc
  (package
    (name "drc")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/drc-fir/drc-fir/"
                           version "/drc-" version ".tar.gz"))
       (sha256
        (base32
         "08ljj4776pjx119zjmfqa8w56bf7x0m7spmi27yk1m455bmiglrj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;there are none
      #:make-flags
      #~(list (string-append "INSTALL_PREFIX=" #$output)
              "-C" "source")
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure))))
    (inputs (list fftw))
    (home-page "https://drc-fir.sourceforge.net/")
    (synopsis "Digital room correction")
    (description
     "DRC is a program used to generate correction filters for acoustic
compensation of HiFi and audio systems in general, including listening room
compensation.  DRC generates just the FIR correction filters, which can be
used with a real time or offline convolver to provide real time or offline
correction.  DRC doesn't provide convolution features, and provides only some
simplified, although really accurate, measuring tools.")
    (license license:gpl2+)))

(define-public bs1770gain
  (package
    (name "bs1770gain")
    (version "0.7.0")
    (home-page "https://manpages.debian.org/sid/bs1770gain/bs1770gain.1.en.html")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/bs1770gain/bs1770gain/"
                           version "/bs1770gain-" version ".tar.gz"))
       (sha256
        (base32 "0a2dcaxvxy5m3a5sb1lhplsymvz3ypaiidc5xn9qy01h53zvyvkp"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; XXX
           (substitute* (find-files "." "\\.[ch]$")
             (("^ \\* N..o.*") ""))
           (substitute* "libbg/bgx.c"
             (("#define BG.* ") "#define BS ")
             (("BG.*NO?.*N.*S.*E.*N.*SE?") "NO")
             (("\"( #|N).*\"") "\"\""))
           (substitute* (list "config.h"
                              "configure.ac"
                              "configure")
             (("https?://bs1770gain[^/]*/")
              ,home-page))
           #t))))
    (build-system gnu-build-system)
    (inputs (list ffmpeg-4 sox))
    (synopsis "Tool to adjust loudness of media files")
    (description
     "BS1770GAIN is a loudness scanner compliant with ITU-R BS.1770 and its
flavors EBU R128, ATSC A/85, and ReplayGain 2.0.  It helps normalizing the
loudness of audio and video files to the same level.")
    (license license:gpl2+)))

(define-public r128gain
  (package
    (name "r128gain")
    (version "1.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/desbma/r128gain.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zqclskkjb9hfdw9gq6iq4bs9dl1wj9nr8v1jz6s885379q9l8i7"))))
    (build-system python-build-system)
    (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'hardcode-ffmpeg
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "r128gain/__init__.py"
                  (("ffmpeg_path or \"ffmpeg\"")
                   (string-append "ffmpeg_path or \""
                                  (search-input-file inputs "bin/ffmpeg")
                                  "\""))))))))
    (inputs (list python-crcmod python-ffmpeg-python python-mutagen
                  python-tqdm ffmpeg))
    (native-inputs (list python-future python-requests))
    (home-page "https://github.com/desbma/r128gain")
    (synopsis "Fast audio loudness scanner & tagger")
    (description
     "r128gain is a multi platform command line tool to scan your audio
files and tag them with loudness metadata (ReplayGain v2 or Opus R128 gain
format), to allow playback of several tracks or albums at a similar
loudness level. r128gain can also be used as a Python module from other
Python projects to scan and/or tag audio files.")
    ;; 'setup.py' claims LGPL2+, 'LICENSE' is LGPLv2.1.
    (license license:lgpl2.1+)))

(define-public filteraudio
  (let ((revision "1")
        (commit "2fc669581e2a0ff87fba8de85861b49133306094"))
    (package
      (name "filteraudio")
      (version (string-append "0.0.0-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/irungentoo/filter_audio")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0hbb290n3wb23f2k692a6bhc23nnqmxqi9sc9j15pnya8wifw64g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" %output)
                            (string-append "CC=" ,(cc-for-target)))
         #:tests? #f ; No tests
         #:phases
         (modify-phases %standard-phases
           ;; No configure script
           (delete 'configure))))
      (synopsis "Lightweight audio filtering library")
      (description "An easy to use audio filtering library made from webrtc
code, used in @code{libtoxcore}.")
      (home-page "https://github.com/irungentoo/filter_audio")
      (license license:bsd-3))))

(define-public gsm
  (package
    (name "gsm")
    (version "1.0.20")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.quut.com/" name "/" name
                       "-" version ".tar.gz"))
       (sha256
        (base32 "1gwhmqs24c14gc9qr91iqb2jkbr3qqy4dvf27yf8j7mq322w65b3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "tst"
       #:make-flags (list (string-append "INSTALL_ROOT=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-fpic-ccflag
           (lambda _
             ;; The -fPIC compiler option is needed when building
             ;; mediastreamer.
             (substitute* "Makefile"
               (("^CCFLAGS.*" all)
                (string-append all "CCFLAGS += -fPIC\n")))))
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/inc"))
               (mkdir-p (string-append out "/man"))
               (mkdir-p (string-append out "/man/man1"))
               (mkdir-p (string-append out "/man/man3"))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/lib")))))
         (add-after 'install 'post-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (rename-file (string-append out "/inc")
                            (string-append out "/include"))
               (mkdir-p (string-append out "/include/gsm"))
               (copy-recursively "inc"
                                 (string-append out "/include/gsm")))))
         (delete 'configure))))         ; no configure script
    (synopsis "GSM 06.10 lossy speech compression library")
    (description "This C library provides an encoder and a decoder for the GSM
06.10 RPE-LTP lossy speech compression algorithm.")
    (home-page "https://quut.com/gsm/")
    (license (license:non-copyleft "file://COPYRIGHT"))))

(define-public python-pyalsaaudio
  (package
    (name "python-pyalsaaudio")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyalsaaudio" version))
              (sha256
               (base32
                "1180ypn9596rq4b7y7dyv627j1q0fqilmkkrckclnzsdakdgis44"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                   ; tests require access to ALSA devices.
    (inputs
     (list alsa-lib))
    (home-page "https://larsimmisch.github.io/pyalsaaudio/")
    (synopsis "ALSA wrappers for Python")
    (description
     "This package contains wrappers for accessing the ALSA API from Python.
It is currently fairly complete for PCM devices, and has some support for
mixers.")
    (license license:psfl)))

(define-public ldacbt
  (package
    (name "ldacbt")
    (version "2.0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/EHfive/ldacBT"
                                  "/releases/download/v" version
                                  "/ldacBT-" version ".tar.gz"))
              (sha256
               (base32
                "1d65dms4klzql29abi15i90f41h523kl6mxrz9hi6p5vg37fxn2b"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (home-page "https://github.com/EHfive/ldacBT/")
    (synopsis "LDAC Bluetooth encoder and ABR library")
    (description "This package provides an encoder for the LDAC
high-resolution Bluetooth audio streaming codec for streaming at up to 990
kbps at 24 bit/96 kHz.")
    (license license:asl2.0)))

(define-public bluez-alsa
  (package
    (name "bluez-alsa")
    (version "3.0.0")
    (source (origin
              ;; The tarballs are mere snapshots and don't contain a
              ;; bootstrapped build system.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Arkq/bluez-alsa")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jlsgxyqfhncfhx1sy3ry0dp6p95kd4agh7g2b7g51h0c4cv74h8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-ldac"
             (string-append "--with-alsaplugindir="
                            (assoc-ref %outputs "out")
                            "/lib/alsa-lib")
             (string-append "--with-dbusconfdir="
                            (assoc-ref %outputs "out")
                            "/etc/dbus-1/system.d"))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list alsa-lib
           bluez
           dbus
           glib
           ldacbt
           libbsd
           ncurses
           ortp
           sbc))
    (home-page "https://github.com/Arkq/bluez-alsa")
    (synopsis "Bluetooth ALSA backend")
    (description "This project is a rebirth of a direct integration between
Bluez and ALSA.  Since Bluez >= 5, the built-in integration has been removed
in favor of 3rd party audio applications.  From now on, Bluez acts as a
middleware between an audio application, which implements Bluetooth audio
profile, and a Bluetooth audio device.  BlueALSA registers all known Bluetooth
audio profiles in Bluez, so in theory every Bluetooth device (with audio
capabilities) can be connected.  In order to access the audio stream, one has
to connect to the ALSA PCM device called @code{bluealsa}.  The device is based
on the ALSA software PCM plugin.")
    (license license:expat)))

(define-public snd
  (package
    (name "snd")
    (version "25.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ccrma-ftp.stanford.edu/pub/Lisp/"
                                  "snd-" version ".tar.gz"))
              (sha256
               (base32
                "0fgxqk0byxdj6059mb9d5qic2dqjabz49j0szsrn2y3c9nz6iyq4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:out-of-source? #f               ; for the 'install-doc' phase
      #:configure-flags
      #~(let ((docdir (string-append #$output "/share/doc/"
                                     #$name "-" #$version)))
          (list "--with-alsa"
                "--with-jack"
                "--with-gmp"
                "--with-gui"
                (string-append "--with-doc-dir=" docdir)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-s7
            (lambda _
              (install-file "s7.h" (string-append #$output "/include"))))
          (add-after 'install 'install-doc
            (lambda _
              (let ((doc (string-append #$output "/share/doc/"
                                        #$name "-" #$version)))
                (for-each
                 (lambda (f)
                   (install-file f doc))
                 (find-files "." "\\.html$"))
                (copy-recursively "pix" (string-append doc "/pix"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           fftw
           flac
           gmp
           gsl
           jack-2
           libsamplerate
           motif
           mpc
           mpfr
           mpg123
           speex
           timidity++
           vorbis-tools
           wavpack))
    (synopsis "Sound editor")
    (home-page "https://ccrma.stanford.edu/software/snd/")
    (description
     "Snd is a sound editor modelled loosely after Emacs.  It can be
customized and extended using either the s7 Scheme implementation (included in
the Snd sources), Ruby, or Forth.")
    (license (license:non-copyleft "file://COPYING"))))

(define-public noise-repellent
  (package
    (name "noise-repellent")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lucianodato/noise-repellent")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hb89x9i2knzan46q4nwscf5zmnb2nwf4w13xl2c0y1mx1ls1mwl"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out")
                            "/lib/lv2"))))
    (inputs
     (list lv2 fftwf))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/lucianodato/noise-repellent")
    (synopsis "LV2 plugin for broadband noise reduction")
    (description "Noise Repellent is an LV2 plugin to reduce noise.  It has
the following features:

@enumerate
@item Spectral gating and spectral subtraction suppression rule
@item Adaptive and manual noise thresholds estimation
@item Adjustable noise floor
@item Adjustable offset of thresholds to perform over-subtraction
@item Time smoothing and a masking estimation to reduce artifacts
@item Basic onset detector to avoid transients suppression
@item Whitening of the noise floor to mask artifacts and to recover higher
  frequencies
@item Option to listen to the residual signal
@item Soft bypass
@item Noise profile saved with the session
@end enumerate
")
    (license license:lgpl3+)))

(define-public lv2-speech-denoiser
  (let ((commit "04cfba929630404f8d4f4ca5bac8d9b09a99152f")
        (revision "1"))
    (package
      (name "lv2-speech-denoiser")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lucianodato/speech-denoiser/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "189l6lz8sz5vr6bjyzgcsrvksl1w6crqsg0q65r94b5yjsmjnpr4"))))
      (build-system meson-build-system)
      (arguments
       `(;; Using a "release" build is recommended for performance
         #:build-type "release"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-meson-build
             (lambda _
               (substitute* "meson.build"
                 (("install_folder = 'sdenoise.lv2'")
                  "install_folder = 'lib/lv2/sdenoise.lv2'")
                 (("build/manifest.ttl") "../build/manifest.ttl"))
               #t))
           (add-after 'unpack 'build-rnnoise
             (lambda _
               (with-directory-excursion "rnnoise"
                 (let ((old-CFLAGS (getenv "CFLAGS")))
                   (setenv "CFLAGS" "-fvisibility=hidden -fPIC -Wl,--exclude-libs,ALL")
                   (setenv "CONFIG_SHELL" (which "bash"))
                   (invoke "autoreconf" "-vif")
                   (invoke "sh" "configure"
                           "--disable-examples"
                           "--disable-doc"
                           "--disable-shared"
                           "--enable-static")
                   (invoke "make")
                   (setenv "CFLAGS" old-CFLAGS))))))))
      (inputs
       (list lv2))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (home-page "https://github.com/werman/noise-suppression-for-voice")
      (synopsis "Speech denoise LV2 plugin based on Xiph's RNNoise library")
      (description "RNNoise is a library that uses deep learning to apply
noise suppression to audio sources with voice presence.  This package provides
an LV2 audio plugin.")
      (license license:lgpl3+))))

(define-public cli-visualizer
  (package
    (name "cli-visualizer")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dpayne/cli-visualizer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "003mbbwsz43mg3d7llphpypqa9g7rs1p1cdbqi1mbc2bfrc1gcq2"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; TODO: Try using the latest googletest for versions > 1.8.
     (list ;; ("googletest" ,googletest-1.8)
           which))
    (inputs
     (list fftw ncurses pulseaudio))
    (arguments
     '(#:tests? #f
       ;; XXX Enable tests after patching them to use the system googletest.
       ;; #:configure-flags (list "-DVIS_WITH_TESTS=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "../source/examples"
               (delete-file "mac_osx_config")
               (for-each (lambda (file)
                           (install-file file
                                         (string-append
                                          (assoc-ref outputs "out")
                                          "/share/doc")))
                         (find-files ".")))
             #t)))))
    (home-page "https://github.com/dpayne/cli-visualizer/")
    (synopsis "Command-line audio visualizer")
    (description "@code{cli-visualizer} displays fast-Fourier
transforms (FFTs) of the sound being played, as well as other graphical
representations.")
    (license license:expat)))

(define-public cava
  (package
    (name "cava")
    (version "0.10.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/karlstav/cava")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09wwzqynfmdzn77vxxmrw2z0yz95p4zg9cgfp9vnpv70visi98d0"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list alsa-lib fftw ncurses pipewire pulseaudio iniparser
                  sdl2 autoconf-archive))
    (arguments
     (list #:configure-flags
           #~(list (string-append "PREFIX="
                                  #$output)
                   (string-append "FONT_DIR="
                                  #$output "/share/consolefonts"))
           #:make-flags
           #~(let ((lib (string-append #$output "/lib")))
               (list (string-append "cava_LDFLAGS = -L" lib " -Wl,-rpath " lib " -lrt")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'bootstrap
                 (lambda _
                   (setenv "HOME"
                           (getcwd))
                   (invoke "sh" "autogen.sh")))
               (add-before 'build 'make-cava-ldflags
                 (lambda _
                   (mkdir-p (string-append #$output "/lib"))))
               (add-after 'install 'data
                 (lambda _
                   (for-each (lambda (file)
                               (install-file file
                                             (string-append #$output
                                              "/share/doc/examples")))
                             (find-files "example_files")))))))
    (home-page "https://github.com/karlstav/cava")
    (synopsis "Console audio visualizer for ALSA, MPD, and PulseAudio")
    (description "C.A.V.A. is a bar audio spectrum visualizer for the terminal
using ALSA, MPD, PulseAudio, or a FIFO buffer as its input.")
    (license license:expat)))

(define-public fluid-3
  (let ((commit "871c8ce2002e8b3c198f532fdb4fbcce7914f951"))
    (package
      (name "fluid-3")
      (version "2.1")
      (source
       (origin
         (method url-fetch)
         ;; Only one file is required, but the release bundles the whole
         ;; software which is 50MiB as tar and 200MiB unpacked. The website
         ;; directly links the soundfont release to the github file download.
         (uri (string-append "https://github.com/musescore/MuseScore/raw/"
                             commit "/share/sound/FluidR3Mono_GM.sf3"))
         (file-name (string-append name "-" version ".sf3"))
         (sha256
          (base32
           "1hjfg5i15bw9279007xs92zsggjgn4s4k9pc00s851l3kvc6dkfg"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((file (assoc-ref %build-inputs "source"))
                 (out (string-append %output "/share/soundfonts")))
             (mkdir-p out)
             (copy-file file (string-append out "/FluidR3Mono_GM.sf3"))
             #t))))
      (home-page  "https://github.com/musescore/MuseScore/tree/master/share/sound")
      (synopsis "Pro-quality GM soundfont")
      (description "Fluid-3 is Frank Wen's pro-quality GM soundfont.")
      (license license:expat))))

(define-public libfdk
  (package
    (name "libfdk")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mstorsjo/fdk-aac")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fkrnzs78fmj11n9z3l0w53i2fl16jcfiyavwidck9bzmkmsf486"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/mstorsjo/fdk-aac")
    (synopsis "Fraunhofer FDK AAC library")
    (description "FDK is a library for encoding and decoding Advanced Audio
Coding (AAC) format audio, developed by Fraunhofer IIS, and included as part of
Android.  It supports several Audio Object Types including MPEG-2 and MPEG-4 AAC
LC, HE-AAC (AAC LC + SBR), HE-AACv2 (LC + SBR + PS) as well AAC-LD (low delay)
and AAC-ELD (enhanced low delay) for real-time communication.  The encoding
library supports sample rates up to 96 kHz and up to eight channels (7.1
                                                                     surround).")
    (license (license:fsf-free "https://github.com/mstorsjo/fdk-aac/blob/master/NOTICE"
                               "https://www.gnu.org/licenses/license-list.html#fdk"))))

(define-public libfreeaptx
  (package
    (name "libfreeaptx")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/iamthehorker/libfreeaptx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fm5041nd08yzg0m9474g0943lb3x54zmn59b53nhvxan8x22ibq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no tests.
           #:make-flags
           #~(list
              (string-append "PREFIX=" #$output)
              (string-append "LDFLAGS=" "-Wl,-rpath=" #$output "/lib")
              (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure))))
    (home-page "https://github.com/iamthehorker/libfreeaptx")
    (synopsis "aptX codec library")
    (description "libfreeaptx is an implementation of the Audio Processing
Technology codecs aptX and aptX HD, mainly intended for use with an A2DP
bluetooth profile.")
    (license license:lgpl2.1+)))

(define-public libopenshot-audio
  (package
    (name "libopenshot-audio")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenShot/libopenshot-audio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m6a0g6y464ypcza1wfaik77x26lfdmkb5k735f7v8463r7qhd0m"))))
    (build-system cmake-build-system)
    (inputs
     (list alsa-lib
           ;; The following are for JUCE GUI components:
           libx11
           freetype
           libxrandr
           libxinerama
           libxcursor))
    (arguments
     (list
      #:tests? #f                       ; there are no tests
      #:configure-flags
      #~(list (string-append "-DCMAKE_CXX_FLAGS=-I"
                           #$(this-package-input "freetype")
                           "/include/freetype2"))))
    (home-page "https://openshot.org")
    (synopsis "Audio editing and playback for OpenShot")
    (description "OpenShot Audio Library (libopenshot-audio) allows
high-quality editing and playback of audio, and is based on the JUCE
library.")
    (license license:lgpl3+)))

(define-public faudio
  (package
    (name "faudio")
    (version "22.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FNA-XNA/FAudio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jdfslxgzysqy0r3nfbsyj6dz0n36fncbsczm9zznxv5phic7g87"))))
    (arguments
     '(#:tests? #f                      ; No tests.
       #:configure-flags '("-DGSTREAMER=ON")))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gstreamer gst-plugins-base sdl2))
    (home-page "https://github.com/FNA-XNA/FAudio")
    (synopsis "XAudio reimplementation")
    (description "FAudio is an XAudio reimplementation that focuses solely on
developing fully accurate DirectX Audio runtime libraries.")
    (license
     (list license:zlib
           ;; stb & utils/{ui,wav}common are dual-licenced under either of:
           license:expat
           license:public-domain))))

(define-public gnaural
  (package
    (name "gnaural")
    (version "20110606")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnaural/Gnaural/gnaural_"
                           version ".tar.xz"))
       (sha256
        (base32
         "1gq519c0imsh57zklyi0f8h64l3ai48lh672c834470z8c6kvbfi"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib gtk+-2 libsndfile portaudio))
    (native-inputs
     (list pkg-config))
    (home-page "https://gnaural.sourceforge.net/")
    (synopsis "Binaural beat synthesizer")
    (description "Gnaural is a programmable auditory binaural beat synthesizer
intended to be used for brainwave entrainment.  Gnaural supports creation of
binaural beat tracks of different frequencies and exporting of tracks into
different audio formats.  Gnaural can also be linked over the internet with
other Gnaural instances, allowing synchronous sessions between many users.")
    (license license:gpl2+)))

(define-public darkice
  (package
    (name "darkice")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rafael2k/darkice/releases/"
                                  "download/v" version "/darkice-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05yq7lggxygrkd76yiqby3msrgdn082p0qlvmzzv9xbw8hmyra76"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list lame
                  libvorbis
                  opus
                  twolame
                  alsa-lib
                  pulseaudio
                  jack-1
                  libsamplerate))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lame-prefix="
                            (assoc-ref %build-inputs "lame")))
       #:make-flags
       (list "CXXFLAGS += -std=gnu++14")))
    (home-page "http://www.darkice.org/")
    (synopsis "Live audio streamer")
    (description "DarkIce is a live audio streamer.  It takes audio input from
a sound card, encodes it into Ogg Vorbis and/or mp3, and sends the audio
stream to one or more IceCast and/or ShoutCast servers.")
    (license license:gpl3+)))

(define-public libltc
  (package
    (name "libltc")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/x42/libltc/releases/download/v"
                       version "/libltc-" version ".tar.gz"))
       (sha256
        (base32 "0j8j7cnw02arh8122d13bwkps1c0mi2xqq55gyi2bs91dk6l4v8a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))           ;tests fail otherwise
    (native-inputs
     (list doxygen pkg-config))
    (synopsis "Encode or decode Linear/Longitudinal Time Code (LTC) audio")
    (description "Libltc is a POSIX-C Library for handling
@dfn{Linear/Longitudinal Time Code} (LTC) data.")
    (home-page "https://x42.github.io/libltc/")
    (license license:lgpl3+)))

(define-public ttaenc
  (package
    (name "ttaenc")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tta/"
                           "tta/ttaenc-src"
                           "/ttaenc-" version "-src.tgz"))
       (sha256
        (base32
         "1iixpr4b89g9g1hwn8ak8k8iflcww3r5f09a117qdidc2nqcijdj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "INSDIR=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure
         (add-before 'install 'make-bindir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               #t))))))
    (synopsis "TTA lossless audio encoder")
    (description
     "TTA performs lossless compression on multichannel 8,16 and 24 bits
data of the Wav audio files.  Being lossless means that no data-
quality is lost in the compression - when uncompressed, the data will
be identical to the original.  The compression ratios of TTA depend on
the type of music file being compressed, but the compression size
will generally range between 30% - 70% of the original.  TTA format
supports both of ID3v1/v2 and APEv2 tags.")
    (home-page "http://tausoft.org/")
    (license license:gpl2+)))

(define-public libsoundio
  (package
    (name "libsoundio")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andrewrk/libsoundio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12l4rvaypv87vigdrmjz48d4d6sq4gfxf5asvnc4adyabxb73i4x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;no tests included
    (inputs
     (list alsa-lib jack-1 pulseaudio))
    (native-inputs
     (list pkg-config))
    (home-page "http://libsound.io")
    (synopsis "C library for real-time audio input and output")
    (description "@code{libsoundio} is a C library providing audio input and
output.  The API is suitable for real-time software such as digital audio
workstations as well as consumer software such as music players.")
    (license license:expat)))

(define-public redkite
  (package
    (name "redkite")
    (version "1.3.1")                     ;marked unmaintained as of Oct. 2021
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/free-sm/redkite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zb2k2a4m7z2ravqrjn8fq8lic20wbr2m8kja3p3113jsk7j9zvd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests included
    (propagated-inputs
     (list cairo))
    (native-inputs
     (list pkg-config))
    (synopsis "Small GUI toolkit")
    (description "Redkite is a small GUI toolkit developed in C++17 and
inspired from other well known GUI toolkits such as Qt and GTK.  It is
minimal on purpose and is intended to be statically linked to applications,
therefore satisfying any requirements they may have to be self contained,
as is the case with audio plugins.")
    (home-page "https://gitlab.com/geontime/redkite")
    (license license:gpl3+)))

(define-public carla
  (package
    (name "carla")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/falkTX/Carla")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01ngkmfcxyg1bb4qmfvlkkjbx4lx62akxqhizl8zmqnhfcy4p9bx"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no "check" target
           #:make-flags
           #~(list (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)      ; no configure script
               (add-before 'build 'set-CC-variable-and-show-features
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (invoke "make" "features")))
               (add-after 'install 'make-carla-executable
                 (lambda _
                   (chmod (string-append #$output "/share/carla/carla") #o555)))
               (add-after 'install 'wrap-executables
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-script (string-append #$output "/bin/carla")
                                #:guile (search-input-file inputs "bin/guile")
                                `("GUIX_PYTHONPATH" ":" prefix
                                  (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs
     (list alsa-lib
           ffmpeg
           fluidsynth
           file
           liblo
           libsndfile
           libx11
           gtk+-2              ;needed for bridging GTK2 plugins in GTK3 hosts
           gtk+
           python-pyliblo
           python-pyqt
           python-rdflib
           ;; python-pyqt shows the following error without python-wrapper:
           ;; Error while finding module specification for 'PyQt5.uic.pyuic'
           ;; (ModuleNotFoundError: No module named 'PyQt5')
           python-wrapper
           qtbase-5
           zlib

           ;; For WRAP-SCRIPT above.
           guile-2.2))
    (native-inputs
     (list pkg-config))
    (home-page "https://kx.studio/Applications:Carla")
    (synopsis "Audio plugin host")
    (description "Carla is a modular audio plugin host, with features like
transport control, automation of parameters via MIDI CC and remote control
over OSC.  Carla currently supports LADSPA (including LRDF), DSSI, LV2, VST2,
and VST3 plugin formats, plus SF2 and SFZ file support.  It uses JACK as the
default and preferred audio driver but also supports native drivers like ALSA.")
    (license license:gpl2+)))

;;; This package variant tracks the latest in-development 2.6 release.
(define-public carla-2.6
  (let ((commit "aa400535b31c67f4b6c1b28e6e20e4d4f82111a3")
        (revision "0"))
    (package
      (inherit carla)
      (name "carla")
      (version (git-version "2.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/falkTX/Carla")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0cnj2sgr60f5h6wdfmihc214wf3n74686sipl3iyzmylqrcyhbjn")))))))

(define-public ecasound
  (package
    (name "ecasound")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nosignal.fi/download/ecasound-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1m7njfjdb7sqf0lhgc4swihgdr4snkg8v02wcly08wb5ar2fr2s6"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    ;; It would be nice to add mikmod to inputs if that gets packaged
    ;; eventually.
    (inputs (list alsa-lib
                  jack-1
                  mpg123
                  lame
                  vorbis-tools
                  faad2
                  flac
                  timidity++
                  libsndfile
                  libsamplerate
                  ncurses
                  ladspa
                  lilv))
    (home-page "https://nosignal.fi/ecasound/index.php")
    (synopsis "Multitrack audio processing")
    (description "Ecasound is a software package designed for multitrack audio
processing.  It can be used for simple tasks like audio playback, recording and
format conversions, as well as for multitrack effect processing, mixing,
recording and signal recycling.  Ecasound supports a wide range of audio inputs,
outputs and effect algorithms.  Effects and audio objects can be combined in
various ways, and their parameters can be controlled by operator objects like
oscillators and MIDI-CCs.  A versatile console mode user-interface is included
in the package.")
    ;; As an exception to the above, the C, C++ and python implementations
    ;; of the Ecasound Control Interface (ECI) are licensed under the LGPL
    ;; (see the file 'COPYING.LGPL'). This allows writing ECI applications
    ;; that are not licensed under GPL.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public libaudec
  (package
    (name "libaudec")
    (version "0.3.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.zrythm.org/zrythm/libaudec")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "02hhhpcfkycicygh6g9hzps255zkbbi33vks6yv6zk5wp9p2nspj"))))
   (build-system meson-build-system)
   (arguments
    `(#:configure-flags
      ;; Build the tests.
      `("-Dtests=true")))
   (inputs
    (list libsamplerate libsndfile))
   (native-inputs
     (list pkg-config))
   (synopsis "Library for reading and resampling audio files")
   (description "libaudec is a wrapper library over ffmpeg, sndfile and
libsamplerate for reading and resampling audio files, based on Robin Gareus'
@code{audio_decoder} code.")
   (home-page "https://git.zrythm.org/zrythm/libaudec")
   (license license:agpl3+)))

(define-public lv2lint
  (package
    (name "lv2lint")
    (version "0.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.open-music-kontrollers.ch/lv2/lv2lint")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1jrka0hsn4n1clri7zfkcl3c2vi52144lkpjm81l51ff8rqy8ks1"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       `("-Delf-tests=true" ; for checking symbol visibility
         "-Donline-tests=true"))) ; for checking URI existence
    (inputs
      (list curl libelf lilv))
    (native-inputs
      (list pkg-config))
    (synopsis "LV2 plugin lint tool")
    (description "lv2lint is an LV2 lint-like tool that checks whether a
given plugin and its UI(s) match up with the provided metadata and adhere
to well-known best practices.")
    (home-page "https://open-music-kontrollers.ch/lv2/lv2lint/")
    (license license:artistic2.0)))

(define-public lv2toweb
  (package
    (name "lv2toweb")
    (version "0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/x42/lv2toweb")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "007aysqvgkf25za8nkmyd5g9kp1zla460dcpidlj5xg1zc3fcdfi"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f  ; no "check" target
      #:make-flags (list "CC=gcc"
                         (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
    (inputs
      (list jalv lilv))
    (native-inputs
      (list help2man pkg-config))
    (synopsis "Documentation generator for LV2 plugins")
    (description
      "lv2toweb allows the user to create an xhtml page with information
about the given LV2 plugin, provided that the plugin and its UI(s) match up
with the provided metadata and adhere to well-known best practices.")
    (home-page "https://github.com/x42/lv2toweb")
    (license (list license:isc license:gpl2))))

(define-public ztoolkit
  (package
    (name "ztoolkit")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.zrythm.org/zrythm/ztoolkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1k60zklrrnch4l0iyzwb4q0srdj3gggwq8cpldwgdhn26ddqkl0d"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    ;; These are listed as propagated inputs because they are dependencies
    ;; in pkgconfig.
    (propagated-inputs
     (list cairo libx11))
    (synopsis "GUI toolkit for LV2 plugins")
    (description "ZToolkit (Ztk) is a cross-platform GUI toolkit heavily
inspired by GTK.  It handles events and low level drawing on behalf of
the user and provides a high-level API for managing the UI and custom
widgets.  ZToolkit is written in C and was created to be used for building
audio plugin UIs, where the dependencies often need to be kept to a
minimum.")
    (home-page "https://git.zrythm.org/zrythm/ztoolkit")
    (license license:agpl3+)))

(define-public ztoolkit-rsvg
  (package/inherit ztoolkit
    (name "ztoolkit-rsvg")
    (arguments
     (list #:configure-flags '(list "-Denable_rsvg=true")))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs ztoolkit)
       (prepend (librsvg-for-system))))
    (synopsis "ZToolkit with SVG support")))

(define-public libinstpatch
  (package
    (name "libinstpatch")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swami/libinstpatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w3nk0vvd1cxic70n45zjip0bdsrja969myvyvkhq3ngbarbykir"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;there are no tests
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib libsndfile))
    (home-page "http://www.swamiproject.org/")
    (synopsis "Instrument file software library")
    (description
     "libInstPatch is a library for processing digital sample based MIDI
instrument \"patch\" files.  The types of files libInstPatch supports are used
for creating instrument sounds for wavetable synthesis.  libInstPatch provides
an object framework (based on GObject) to load patch files, which can then be
edited, converted, compressed and saved.")
    (license license:lgpl2.1)))

(define-public lsp-dsp-lib
  (package
    (name "lsp-dsp-lib")
    (version "1.0.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/lsp-plugins/lsp-dsp-lib/"
                            "releases/download/" version
                            "/lsp-dsp-lib-src-" version ".tar.gz"))
        (sha256
         (base32 "07g02nglzrq9yp267m1aflrmr7i35pc3anlhasp35048i0xvy51i"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'omit-static-library
                 (lambda _
                   (substitute* "src/Makefile"
                     ((".*cp \\$\\(ARTIFACT_SLIB\\).*") "") ; don't install it
                     ((" \\$\\(ARTIFACT_SLIB\\)") ""))))    ; don't build it
               (replace 'configure
                 (lambda _
                   (invoke "make" "config"
                           (string-append "PREFIX=" #$output)))))))
    (home-page "https://github.com/lsp-plugins/lsp-dsp-lib")
    (synopsis "Digital signal processing library")
    (description "The LSP DSP library provides a set of functions that perform
SIMD-optimized computing on several hardware architectures.  All functions
currently operate on IEEE-754 single-precision floating-point numbers.")
    (license license:lgpl3+)))

(define-public codec2
  (package
    (name "codec2")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/drowe67/codec2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jpvr7bra8srz8jvnlbmhf8andbaavq5v01qjnp2f61za93rzwba"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bc octave valgrind))
    (arguments
     `(#:tests? #f ; TODO: Fix tests (paths, graphic toolkit, octave modules).
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-test-environment
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (synopsis "Speech codec")
    (description
     "Codec 2 is a speech codec designed for communications quality speech
between 700 and 3200 bit/s.  The main application is low bandwidth HF/VHF
digital radio.")
    (home-page "https://www.rowetel.com/?page_id=452")
    (license license:lgpl2.1)))

(define-public mbelib
  ;; No release since 2016, use commit directly.
  (let ((commit "9a04ed5c78176a9965f3d43f7aa1b1f5330e771f")
        (revision "1"))
    (package
      (name "mbelib")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/szechyjs/mbelib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a7xmf87xnjzm5b437j2vnwv39x0ascja1j04c5wj6xs1529gw8h"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/szechyjs/mbelib")
      (synopsis "P25 Phase 1 and ProVoice vocoder")
      (description
       "The mbelib library provides support for the 7200x4400 bit/s codec used
in P25 Phase 1, the 7100x4400 bit/s codec used in ProVoice and the @emph{Half
Rate} 3600x2250 bit/s vocoder used in various radio systems.")
      (license (list license:bsd-3      ; test/ framework
                     license:isc)))))   ; the rest

(define-public ableton-link
  (package
    (name "ableton-link")
    (version "3.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ableton/link")
                    (commit (string-append "Link-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wplqj11ww64gmw2kzlxpvfs3v04m2036f7k5ndm34zcv12b91fa"))
              (modules '((guix build utils)))
              (patches
               (search-patches "ableton-link-system-libraries-debian.patch"))
              (snippet
               '(begin
                  ;; Tests assume that CMake's "build" directory is a
                  ;; sub-directory of the source tree, so we fix it.
                  (substitute* "ci/run-tests.py"
                    (("root_dir,") "root_dir, os.pardir,"))
                  ;; Unbundle dependencies.
                  (delete-file-recursively "third_party")
                  (delete-file-recursively "modules")))))
    (build-system cmake-build-system)
    (native-inputs
     (list catch-framework
           python ;for running tests
           portaudio ;for portaudio examples
           qtbase-5 ;for Qt examples
           qtdeclarative-5
           qttools-5))
    (inputs
     (list jack-1 ;for JACK examples
           qtquickcontrols-5)) ;for Qt examples
    (propagated-inputs
     ;; This is because include/ableton/platforms/asio/AsioWrapper.hpp
     ;; contains '#include <asio.hpp>'.
     (list asio))
    (arguments
     `(#:configure-flags
       '("-DLINK_BUILD_QT_EXAMPLES=ON"
         "-DLINK_BUILD_JACK=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (when tests?
               (let* ((python (search-input-file inputs "/bin/python3"))
                      (run-tests "../source/ci/run-tests.py"))
                 (invoke python run-tests "--target" "LinkCoreTest")
                 (invoke python run-tests "--target" "LinkDiscoveryTest")))))
         (add-before 'install 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((source "../source/"))
               (substitute* (string-append source
                                           "cmake_include/AsioStandaloneConfig.cmake")
                 (((string-append "\\$\\{CMAKE_CURRENT_LIST_DIR\\}/\\.\\./"
                                  "modules/asio-standalone/asio/include"))
                  (string-append (assoc-ref inputs "asio")
                                 "/include")))
               (substitute* (string-append source "AbletonLinkConfig.cmake")
                 (("\\$\\{CMAKE_CURRENT_LIST_DIR\\}/include")
                  "${CMAKE_CURRENT_LIST_DIR}/../../../include")
                 (("\\$\\{CMAKE_CURRENT_LIST_DIR\\}/include/ableton/Link\\.hpp")
                  "${CMAKE_CURRENT_LIST_DIR}/../../../include/ableton/Link.hpp")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib-cmake (string-append out "/lib/cmake/ableton-link"))
                    (source "../source"))
               (for-each (lambda (test-file)
                           (delete-file test-file))
                         '("bin/LinkDiscoveryTest" "bin/LinkCoreTest"))
               (copy-recursively "bin" bin)
               (copy-recursively (string-append source "/include/ableton")
                                 (string-append out "/include/ableton"))
               (install-file (string-append source "/AbletonLinkConfig.cmake")
                             lib-cmake)
               (install-file (string-append source
                                            "/cmake_include/AsioStandaloneConfig.cmake")
                             (string-append lib-cmake "/cmake_include"))))))))
    (home-page "https://github.com/Ableton/link")
    (synopsis "Synchronize musical beat, tempo, and phase across multiple applications")
    (description
     "Ableton Link is a C++ library that synchronizes musical beat, tempo, and phase
across multiple applications running on one or more devices.  Applications on devices
connected to a local network discover each other automatically and form a musical
session in which each participant can perform independently: anyone can start or stop
while still staying in time.")
    (license license:gpl2+)))

(define-public butt
  (package
    (name "butt")
    (version "0.1.38")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/butt/butt/butt-"
                                  version "/butt-" version ".tar.gz"))
              (sha256
               (base32
                "10i3xpxzccdl4pidiyymw9cfavhy50yhn7xi5bd77y91f2903kp9"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "src/butt.cpp"
                  ((".*zica.*") "")))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-documentation
                 (lambda _
                   (let ((doc (string-append #$output "/share/doc/" #$name)))
                     (install-file "README" doc)
                     (copy-file #$(this-package-native-input "manual")
                                (string-append doc "/butt-manual.pdf"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("manual"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://danielnoethen.de/butt/butt-"
                               version "_manual.pdf"))
           (sha256
            (base32 "04aixxqshfj11ja3ifh0zvywl2mqzmymppcd0xj8sv0j7whjibaq"))))))
    (inputs
     (list curl
           dbus
           flac
           fltk-1.3
           lame
           libfdk
           libsamplerate
           libvorbis
           libx11
           libxext
           libxfixes
           libxft
           libxrender
           libogg
           openssl
           opus
           portaudio))
    (home-page "https://danielnoethen.de/butt/")
    (synopsis "Audio streaming tool")
    (description "Butt is a tool to stream audio to a ShoutCast or
Icecast server.")
    (license license:gpl2+)))

(define-public siggen
  (package
    (name "siggen")
    (version "2.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bleskodev/siggen")
             (commit "a407611b59d59c7770bbe62ba9b8e9a948cf3210")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0szhgfd9kddr6qsz0imp0x66jjn6ry236f35vjl82ivc1v2bllcb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "INSDIR=" %output "/bin")
                          (string-append "MANDIR=" %output "/share/man"))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; Patch misc.c to prevent a segfault.
         (add-after 'unpack 'patch-segfault
           (lambda _
             (substitute* "misc.c"
               (("#include <stdio.h>\n" all)
                (string-append all "#include <string.h>\n")))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/bin" "/share/man/man1" "/share/man/man5"))
               (apply invoke "make" "sysinstall" make-flags)))))))
    (inputs
     (list ncurses))
    (native-inputs
     `(("groff" ,groff-minimal)         ; for nroff
       ("util-linux" ,util-linux)))     ; for col
    (home-page "https://github.com/bleskodev/siggen")
    (synopsis "Signal generation tools")
    (description "siggen is a set of tools for imitating a laboratory signal
generator, generating audio signals out of Linux's /dev/dsp audio
device.  There is support for mono and/or stereo and 8 or 16 bit samples.")
    (license license:gpl2)))

(define-public python-pysox
  ;; PyPi does not include the data folder containing audio files for testing.
  (let ((commit "3d0053381c24ae3490f759d4de87194b85789d36")
        (revision "0"))
    (package
      (name "python-pysox")
      (version (git-version "1.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rabitt/pysox")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0i62jx92vfpcr2z7lp69yzqdi9idfs3pifl3rzm2akc2c4cr1mac"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-sox
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((sox-store-path (assoc-ref inputs "sox"))
                      (sox-bin (string-append sox-store-path "/bin/sox")))
                 (substitute* "sox/__init__.py"
                   (("sox -h")
                    (string-append sox-bin " -h")))
                 (substitute* "sox/core.py"
                   (("\\['sox")
                    (string-append "['" sox-bin))))))
           (replace 'check
             (lambda* (#:key inputs outputs tests? #:allow-other-keys)
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "pytest")))))))
      (propagated-inputs
       (list python-numpy python-typing-extensions))
      (native-inputs
       (list sox python-pytest python-pytest-cov python-soundfile))
      (home-page "https://github.com/rabitt/pysox")
      (synopsis "Python wrapper around SoX")
      (description "@code{python-pysox} is a wrapper around the @command{sox}
command line tool.  The API offers @code{Transformer} and @code{Combiner}
classes that allow the user to incrementally build up effects and audio
manipulations.  @code{python-pysox} also provides methods for querying audio
information such as sample rate, determining whether an audio file is silent,
and much more.")
      (license license:bsd-3))))

(define-public python-resampy
  (package
    (name "python-resampy")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         ;; PyPi does not include tests.
         (url "https://github.com/bmcfee/resampy")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dlm9ksm7yzgg582sic0vqwfcwdya1g4gnydxldfhaq4y0wakr9c"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numba python-numpy python-scipy python-six))
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
    (home-page "https://github.com/bmcfee/resampy")
    (synopsis "Efficient signal resampling")
    (description
     "@code{python-resampy} implements the band-limited sinc interpolation
method for sampling rate conversion as described by Julius O. Smith at the
@url{https://ccrma.stanford.edu/~jos/resample/, Digital Audio Resampling
Home Page}.")
    (license license:isc)))

(define-public python-librosa
  (package
    (name "python-librosa")
    (version "0.10.2.post1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/librosa/librosa/")
             (commit version)
             ;; For test files.
             (recursive? #true)))
       (sha256
        (base32 "1x37148y1rh4sq2nc59iw9jlza3zwawxnlb7bd9w36an05aclmnh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Ignore --mpl flag.
      '(list "-c" "/dev/null"
             "-k" (string-append
                   ;; Resampling tests require python-samplerate.
                   "not resample"
                   ;; These tests use Pooch and download data files.
                   " and not example and not test_cite"
                   ;; XXX assert 22050 == 31744
                   " and not test_stream"))))
    (propagated-inputs
     (list python-audioread
           python-decorator
           python-joblib
           python-lazy-loader
           python-msgpack
           python-numba
           python-numpy
           python-pooch
           python-scikit-learn
           python-scipy
           python-soundfile
           python-soxr
           python-typing-extensions))
    (native-inputs
     (list python-matplotlib
           python-packaging
           python-pytest
           python-pytest-cov
           python-resampy
           python-setuptools
           python-wheel))
    (home-page "https://librosa.org")
    (synopsis "Python module for audio and music processing")
    (description
     "@code{librosa} is a python package for music and audio analysis.  It
provides the building blocks necessary to create music information retrieval
systems.")
    (license license:isc)))

(define-public mda-lv2
  (package
    (name "mda-lv2")
    (version "1.2.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.drobilla.net/mda-lv2-"
                            version ".tar.xz"))
        (sha256
         (base32 "0nm7qahkrxjydv1wii46ca6948srwhjilhlp54z9bpcnln35ksmf"))))
    (build-system meson-build-system)
    (inputs
     (list lv2))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/mda-lv2.html")
    (synopsis "Audio plug-in pack for LV2")
    (description
     "MDA-LV2 is an LV2 port of the MDA plugins.  It includes effects and a few
instrument plugins.")
    (license license:gpl3+)))

(define-public libodiosacd
  (package
   (name "libodiosacd")
   (version "21.8.30")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tari01/libodiosacd")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0iamf7wksbql0qfigdv5ahaax53ms2yligdav8dw6x0ay88x4lhi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("\\$\\(DESTDIR\\)/usr")
                "\\$(DESTDIR)"))))
        (delete 'configure)) ; no configure script
      #:make-flags
      (list (string-append "DESTDIR=" %output))))
   (synopsis "Library for decoding Super Audio CDs (SACD)")
   (description
    "The Odio SACD shared library is a decoding engine which takes a Super
Audio CD source and extracts a 24-bit high resolution WAV file.  It handles
both DST and DSD streams.")
   (home-page "https://tari.in/www/software/libodiosacd/")
   (license license:gpl3+)))

(define-public odio-sacd
  (package
   (name "odio-sacd")
   (version "21.1.9")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tari01/odio-sacd")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0314srqk0r4qv292qiaply619l2fw04nkdwvqhj3q1dqzv41g4qk"))))
   (inputs (list libodiosacd))
   ;; Build system and arguments for libodiosacd are identical.
   (build-system (package-build-system libodiosacd))
   (arguments (package-arguments libodiosacd))
   (synopsis "Rip Super Audio CDs (SACD)")
   (description
    "Odio SACD is a command-line application which takes a Super Audio CD
source and extracts a 24-bit high resolution WAV file.  It handles both DST
and DSD streams.")
   (home-page "https://tari.in/www/software/odio-sacd/")
   (license license:gpl3+)))

(define-public qpwgraph
  (package
    (name "qpwgraph")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/rncbc/qpwgraph")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "036qzc2sjxa1lvysf7shyjkp1jyjkpalgxf74bgyzm89phqac7cc"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))                ; no tests
    (inputs (list alsa-lib
                  libxkbcommon
                  pipewire
                  qtbase
                  qtsvg))
    (native-inputs (list pkg-config))
    (synopsis "PipeWire graph manager")
    (description
     "qpwgraph is a graph manager dedicated to PipeWire, using the Qt C++
framework.  It provides a visual interface to audio and video connections
managed by PipeWire.")
    (home-page "https://gitlab.freedesktop.org/rncbc/qpwgraph")
    (license license:gpl2)))

(define-public raysession
  (package
    (name "raysession")
    (version "0.14.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Houston4444/RaySession")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m44n6p192i5cvbj98jkmp4ywmm2bjzdbbipaa9xgg07x0jz1mcr"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;no test target
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:modules '((guix build gnu-build-system) (guix build qt-utils)
                  (guix build utils))
      #:imported-modules (cons '(guix build qt-utils)
                               %default-gnu-imported-modules)
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-build-system
            (lambda _
              (substitute* "Makefile"
                (("\\$\\(DESTDIR\\)/etc/xdg")
                 "$(PREFIX)/etc/xdg"))))
          (add-after 'install 'wrap-scripts
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion (string-append #$output "/share/raysession/src/bin")
                (for-each (lambda (script)
                            (wrap-script script
                              #:guile (search-input-file inputs "bin/guile")
                              `("PYTHONPATH" ":" prefix
                                (,(string-append #$output "/share/raysession/src/gui")
                                 ,(string-append #$output "/share/raysession/src/daemon")
                                 ,(string-append #$output "/share/raysession/src/control")
                                 ,(getenv "GUIX_PYTHONPATH")))))
                          '("raysession"
                            "ray_control"
                            "ray-daemon"
                            "ray-proxy")))))
          (add-after 'wrap-scripts 'wrap-qt
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-all-qt-programs #:outputs outputs #:inputs inputs))))))
    (native-inputs (list qtbase-5 qttools-5 which))
    (inputs
     (list guile-3.0 jack-2 pipewire python-pyliblo3 python-pyqt python-wrapper))
    (home-page "https://github.com/Houston4444/RaySession")
    (synopsis "Audio session manager")
    (description "RaySession is a session manager for audio programs such as
Ardour, Carla, QTractor, Guitarix, Patroneo, Jack Mixer, etc.  The principle
is to load together audio programs, then be able to save or close all
documents together.  Its main purpose is to manage NSM compatible programs,
but it also helps for other programs.  It offers a patchbay for visualizing
and editing connections.")
    (license license:gpl2+)))

(define-public streamripper
  (package
    (name "streamripper")
    (version "1.64.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/streamripper"
                           "/files/streamripper%20(current)/"
                           version "/streamripper-" version ".tar.gz"))
       (sha256
        (base32 "0hnyv3206r0rfprn3k7k6a0j959kagsfyrmyjm3gsf3vkhp5zmy1"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled copy of libmad.
        '(delete-file-recursively "libmad-0.15.1b"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list faad2 glib libmad libvorbis))
    (home-page "https://streamripper.sourceforge.net")
    (synopsis "Record audio streams to your hard drive")
    (description "Streamripper records shoutcast-compatible
streams.  For shoutcast style streams it finds the “meta data” or track
separation data, and uses that as a marker for where the track should
be separated.")
    (license license:gpl2+)))

(define-public cubeb
  (let ((commit "9e29d728b0025c674904f83f5a13a88d1a6a5edc")
        (revision "1"))
    (package
      (name "cubeb")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mozilla/cubeb")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1sxkr3h8a4hd3c3a3cjydrszz6npxk3vh6ra3y67lds3zgc69c7n"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         ;; Sanitizers-cmake requires a git submodule.
         '("-DUSE_SANITIZERS=0"
           ;; Tests require a git submodule for googletest.
           "-DBUILD_TESTS=0"
           ;; Use our speex, not a bundled one.
           "-DBUNDLE_SPEEX=0"
           ;; A static library would be built by default.
           "-DBUILD_SHARED_LIBS=1"
           ;; Explicitly link against audio libraries so they are on the
           ;; runpath.  Otherwise cubeb tries to dlopen them at runtime.
           "-DCMAKE_SHARED_LINKER_FLAGS=-lasound -lpulse -lspeex")
         #:tests? #f))
      (inputs (list alsa-lib pulseaudio speex))
      (synopsis "Cross-platform audio library")
      (description "Cubeb is Mozilla's cross-platform audio library.")
      (home-page "https://github.com/mozilla/cubeb")
      (license license:isc))))

(define-public cyanrip
  (package
    (name "cyanrip")
    (version "0.9.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyanreg/cyanrip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13v6gjbxw6ybviq802wmgwlwy846ma4yw94aay0h698qhjqwf0qq"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list curl
                  ffmpeg
                  libcdio-paranoia
                  libmusicbrainz
                  libxml2
                  neon))
    (synopsis "Command line CD ripper and encoder")
    (description
     "cyanrip is a command line tool for ripping CDs.  It uses
MusicBrainz to name and tag each track, and to download and embed cover art.
cyanrip supports encoding tracks to multiple formats in parallel and automatically
verifies checksums.")
    (home-page "https://github.com/cyanreg/cyanrip")
    (license license:lgpl2.1+)))

(define-public easyeffects
  (package
    (name "easyeffects")
    (version "7.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wwmm/easyeffects")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0vr21d12vbyvw3l1h4dx44fk1vk7ns7l8ynclchw5flhsd58yg3d"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ;for glib-compile-resources
           gcc-12 ; fails to build with gcc-11
           gettext-minimal
           itstool
           pkg-config))
    (inputs
     (list fftwf
           fmt
           gsl
           gtk
           json-modern-cxx ;nlohmann_json
           libadwaita
           libbs2b
           libebur128
           libportal
           libsamplerate
           libsigc++
           libsndfile
           lilv
           pango
           pipewire
           rnnoise
           speexdsp
           tbb
           zita-convolver
           soundtouch))
    ;; Propagating these allows EasyEffects to find the plugins via their
    ;; search-path specification
    (propagated-inputs
     (list lv2
           calf
           `(,lsp-plugins "lv2")
           mda-lv2
           zam-plugins
           ladspa))
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _ ; Remove dependency on needless desktop cache stuff.
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true") "gtk_update_icon_cache: false")
               (("update_desktop_database: true") "update_desktop_database: false")))))))
    (home-page "https://github.com/wwmm/easyeffects")
    (synopsis "Realtime Audio effects interface for Pipewire")
    (description "EasyEffects is an advanced audio manipulation tool providing
a graphical user interface to apply various effects and filters to audio
streams using Pipewire.  Effects can be applied in real time to audio inputs or
outputs such as a microphone to reduce noise or apply many other effects
including:

@itemize
@item Auto gain
@item Bass enhancer
@item Bass loudness
@item Compressor
@item Convolver
@item Crossfeed
@item Crystalizer
@item De-esser
@item Delay
@item Echo Canceller
@item Equalizer
@item Exciter
@item Filter (low-pass, high-pass, band-pass and band-reject modes)
@item Gate
@item Limiter
@item Loudness
@item Maximizer
@item Multiband compressor
@item Multiband gate
@item Noise reduction
@item Pitch
@item Reverberation
@item Speech Processor
@item Stereo tools
@end itemize")
    (license license:gpl3+)))
