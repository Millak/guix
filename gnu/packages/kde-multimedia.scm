;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2021, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
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

(define-module (gnu packages kde-multimedia)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public audiocd-kio
  (package
    (name "audiocd-kio")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/audiocd-kio-" version ".tar.xz"))
       (sha256
        (base32 "1ldw51wly4shk4c9a0lc6j8ax176bb9f1l5r5x6rcgwz1vncd3g4"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list cdparanoia
           flac
           kcmutils
           kconfig
           ki18n
           kio
           libkcddb
           libkcompactdisc
           libvorbis
           phonon))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/kio_audiocd/")
    (synopsis "Transparent audio CD integration for applications using the KDE
Platform")
    (description "KIO AudioCD is a KIO slave that enables KIO-aware
applications (such as Dolphin or k3b) to access audio and CD text data on the
audio compact disks.  It allows transparent drag and drop conversion of audio
data into the popular formats and has a configuration System Settings module
available in the \"Multimedia\" section.

This package is part of the KDE multimedia module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public dragon
  (package
    (name "dragon")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dragon-" version ".tar.xz"))
       (sha256
        (base32 "112bq74sh8axcs8ci07zllzxfvk3c1r34j0gj2jmvj4x5pw6hrgq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list bash-minimal
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kjobwidgets
           knotifications
           kparts
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           breeze-icons ; default icon set
           phonon
           phonon-backend-vlc
           solid))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/dragonplayer/")
    (synopsis "Simple video player")
    (description "Dragon Player is a multimedia player where the focus is on
simplicity, instead of features.  Dragon Player does one thing, and only one
thing, which is playing multimedia files.  It's simple interface is designed
not to get in your way and instead empower you to simply play multimedia
files.

This package is part of the KDE multimedia module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public haruna
  (package
    (name "haruna")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/haruna/" version
                                  "/haruna-" version ".tar.xz"))
              (sha256
               (base32 "0vnzv13m6dfpj3vql0h0rqpdqpnaj0rmfqjjzwvqdk4d2kippd1w"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-yt-dlp-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/application.cpp"
                     (("findExecutable\\(u\"yt-dlp\"")
                      (string-append "findExecutable(u\""
                                     (search-input-file inputs "bin/yt-dlp")
                                     "\""))))))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list breeze ;default theme
           breeze-icons ;default icon set
           ffmpeg
           kcolorscheme
           kconfig
           kcoreaddons
           kcrash
           kfilemetadata
           ki18n
           kiconthemes
           kio
           kirigami
           kwindowsystem
           mpvqt
           qqc2-desktop-style
           sonnet
           qt5compat
           yt-dlp))
    (home-page "https://haruna.kde.org/")
    (synopsis "Video player built with Qt/QML and libmpv")
    (description "Haruna is a video player build with Qt/QML and libmpv.")
    (license license:gpl3+)))

(define-public elisa
  (package
    (name "elisa")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/elisa-" version ".tar.xz"))
       (sha256
        (base32 "1hzzi12j8kz04psdd8w0iqib50lyninni3qrqlx17chwnhysn6ax"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config dbus kdoctools
           xorg-server-for-tests python-minimal))
    (inputs
     (list kconfig
           baloo
           kconfigwidgets
           kcoreaddons
           kcrash
           kcmutils
           kdbusaddons
           kdeclarative
           kfilemetadata
           ki18n
           kiconthemes
           kio
           kirigami
           kirigami-addons
           qqc2-desktop-style
           kparts
           kpackage
           kwidgetsaddons
           kxmlgui
           breeze-icons ; default icon set
           phonon
           qtsvg
           qtdeclarative
           qtmultimedia
           ;; TODO: upnpqt https://gitlab.com/homeautomationqt/upnp-player-qt
           vlc))
    (arguments
     `(#:qtbase ,qtbase
       #:tests? #f ;; many tests fail
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server, setting
             ;; QT_QPA_PLATFORM=offscreen does not suffice.
             (system "Xvfb :1 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")))
         (replace 'check
           (lambda* (#:key tests? test-target #:allow-other-keys)
             (when tests?
               (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
               (invoke "dbus-launch" "make" test-target)))))))
    (home-page "https://apps.kde.org/elisa/")
    (synopsis "Powerful music player for Plasma 5")
    (description "Elisa is a simple music player aiming to provide a nice
experience for its users.  Elisa browses music by album, artist or
all tracks.  The music is indexed using either a private indexer or an indexer
using Baloo.  The private one can be configured to scan music on chosen paths.
The Baloo one is much faster because Baloo is providing all needed data from
its own database.  You can build and play your own playlist.")
    (license license:lgpl3+)))

(define-public ffmpegthumbs
  (package
    (name "ffmpegthumbs")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ffmpegthumbs-" version ".tar.xz"))
       (sha256
        (base32 "1s8dq4cj75cbd1gg80d54lh9p87rpkz1ysbsq44lrjrq5wsg48id"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list ffmpeg kconfig ki18n kio taglib))
    (arguments (list #:qtbase qtbase
                     #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (home-page "https://apps.kde.org/ffmpegthumbs/")
    (synopsis "Video thumbnail generator for KDE using ffmpeg")
    (description "
FFMpegThumbs is a video thumbnail generator for KDE file managers
like Dolphin and Konqueror.  It enables them to show preview images
of video files using FFMpeg.

This package is part of the KDE multimedia module.")
    (license license:gpl2+)))

(define-public juk
  (package
    (name "juk")
    (version "24.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/juk-" version ".tar.xz"))
       (sha256
        (base32 "0ys1zgplfiha0f9dgsf2pwbxnw7i9gmazyzfpdrv4sbp6ii1dgrz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons
           kcompletion
           kconfig
           kcrash
           kdbusaddons
           kglobalaccel
           ki18n
           kiconthemes
           kjobwidgets
           kio
           knotifications
           ktextwidgets
           kstatusnotifieritem
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           breeze-icons ; default icon set
           phonon
           phonon-backend-vlc
           qtbase
           qtsvg
           taglib))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/juk/")
    (synopsis "Music jukebox / music player")
    (description "JuK is a powerful music player capable of managing a large
music collection.

Some of JuK's features include:
@itemize
@item Music collection, playlists, and smart playlists
@item Tag editing support, including the ability to edit multiple files at once
@item Tag-based music file organization and renaming
@item CD burning support using k3b
@item Album art using Google Image Search
@end itemize

This package is part of the KDE multimedia module.")
    (license license:gpl2+)))

(define-public kid3
  (package
    (name "kid3")
    (version "3.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/multimedia/kid3.git/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09pva85ffamjdr6m446jcvxjw8qyy7anmj1gz0fvn9ns3d1jgg46"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-DBUILD_WITH_QT6=ON"
         (string-append "-DDOCBOOK_XSL_DIR="
                        #$(this-package-native-input "docbook-xsl")))
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: Documentation build scripts use unix pipes, which will fail
          ;; in the build environment.
          (add-after 'unpack 'skip-docs
            (lambda _
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(doc\\)") "")))))))
    (native-inputs
     (list docbook-xsl
           extra-cmake-modules
           ffmpeg-4
           kdoctools
           libxslt
           python-minimal-wrapper
           qttools))
    (inputs
     (list chromaprint
           flac
           id3lib
           kconfig
           kconfigwidgets
           kcoreaddons
           kio
           kwidgetsaddons
           kxmlgui
           libvorbis
           qtdeclarative
           qtmultimedia
           readline
           taglib
           zlib))
    (home-page "https://kid3.kde.org/")
    (synopsis "Audio tag editor")
    (description "Kid3 is an audio tag editor for KDE that supports a large
variety of formats.")
    (license license:gpl2+)))

(define-public k3b
  (package
    (name "k3b")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/k3b-" version ".tar.xz"))
       (sha256
        (base32 "11r6nda3djj9p918sx9bpipc1byg5mvgib4vyf0kpdpnh9bnhvcj"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-absolute-library-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Set absolute paths for dlopened libraries. We can’t use k3b’s
              ;; runpath as they are loaded by the Qt library.
              (let ((libcdio-paranoia (assoc-ref inputs "libcdio-paranoia"))
                    (libdvdcss (assoc-ref inputs "libdvdcss")))
                (substitute* "libk3b/tools/k3bcdparanoialib.cpp"
                  (("\"(cdio_cdda|cdio_paranoia)\"" _ library)
                   (string-append "\"" libcdio-paranoia "/lib/" library "\"")))
                (substitute* "libk3b/tools/k3blibdvdcss.cpp"
                  (("\"(dvdcss)\"" _ library)
                   (string-append "\"" libdvdcss "/lib/" library "\""))))))
          (add-before 'configure 'fix-cmake-taglib
            (lambda _
              ;; Use the CMake variables provided by FindTaglib from
              ;; extra-cmake-modules, instead of bundled FindTaglib.cmake:
              (substitute*
                  '("plugins/decoder/mp3/CMakeLists.txt"
                    "plugins/decoder/flac/CMakeLists.txt"
                    "plugins/project/audiometainforenamer/CMakeLists.txt")
                (("TAGLIB_INCLUDES") "Taglib_INCLUDE_DIRS")
                (("TAGLIB_LIBRARIES") "Taglib_LIBRARIES"))))
          (add-after 'qt-wrap 'wrap-path
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Set paths to backend programs.
              (wrap-program (string-append (assoc-ref outputs "out") "/bin/k3b")
                `("PATH" ":" prefix
                  ,(map (lambda (input)
                          (string-append (assoc-ref inputs input) "/bin"))
                        '("cdrdao" "cdrtools" "dvd+rw-tools" "libburn" "sox")))))))))
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools-5))
    (inputs
     (list bash-minimal
           cdrdao
           cdrtools
           dvd+rw-tools
           ffmpeg
           flac
           karchive-5
           kcmutils-5
           kconfig-5
           kcoreaddons-5
           kfilemetadata-5
           ki18n-5
           kiconthemes-5
           kio-5
           kjobwidgets-5
           knewstuff-5
           knotifications-5
           knotifyconfig-5
           kservice-5
           kwidgetsaddons-5
           kxmlgui-5
           lame
           libburn
           libcdio-paranoia
           libdvdcss
           libdvdread
           ;; TODO: LibFuzzer
           libkcddb-qt5
           libmad
           libmpcdec
           ;;("libmusicbrainz" ,libmusicbrainz) ; wants old version 2
           libsamplerate
           libsndfile
           libvorbis
           breeze-icons ; default icon set
           shared-mime-info
           solid-5
           sox
           taglib
           zlib))
    (home-page "https://apps.kde.org/k3b/")
    (synopsis "Sophisticated CD/DVD burning application")
    (description "K3b is CD-writing software which intends to be feature-rich
and provide an easily usable interface.  Features include burning audio CDs
from .WAV and .MP3 audio files, configuring external programs and configuring
devices.

The @code{udisks-service-type} should be enabled for @command{k3b} to discover
the available CD drives.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kaffeine
  (package
    (name "kaffeine")
    (version "2.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kaffeine"
                           "/kaffeine-" version ".tar.xz"))
       (sha256
        (base32 "10dnhr9v2jlki44i3gmjagky66ybixmv6f29z5imk9clgddrlyfr"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools-5))
    (inputs
     (list eudev
           kcoreaddons-5
           kdbusaddons-5
           ki18n-5
           kio-5
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           libxscrnsaver
           breeze-icons ; default icon set
           qtbase-5
           qtx11extras
           solid-5
           v4l-utils ; libdvbv5
           vlc))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-code
                 (lambda _
                   (substitute* "src/dvb/dvbdevice_linux.cpp"
                     (("\\s*qPrintable\\(transponder\\.getTransmissionType\\(\\)\\)\\);")
                      "transponder.getTransmissionType());")))))))
    (home-page "https://apps.kde.org/kaffeine/")
    (synopsis "Versatile media player for KDE")
    (description "Kaffeine is a media player for KDE.  While it supports
multiple Phonon backends, its default backend is Xine, giving Kaffeine a wide
variety of supported media types and letting Kaffeine access CDs, DVDs, and
network streams easily.

Kaffeine can keep track of multiple playlists simultaneously, and supports
autoloading of subtitle files for use while playing video.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kamoso
  (package
    (name "kamoso")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kamoso-" version ".tar.xz"))
       (sha256
        (base32 "1i2w2cmlfr9q4p405kycy2xqp8q5d6f1j3pwr5sbdhis3rm7vm4l"))))
    (build-system qt-build-system)
    (native-inputs
     (list
      extra-cmake-modules
      `(,glib "bin")
      kdoctools-5
      pkg-config))
    (inputs
     (list gstreamer
           gst-plugins-base
           kconfig-5
           ki18n-5
           kio-5
           kirigami-5
           knotifications-5
           kparts-5
           breeze-icons ; default icon set
           purpose-5
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtquickcontrols-5
           qtquickcontrols2-5 ; not listed as dependency
           qtx11extras))
    (arguments
     (list #:tests? #f)) ; test program gets built, but is not found
    (home-page "https://apps.kde.org/kamoso/")
    (synopsis "Take pictures and videos out of your webcam")
    (description "Kamoso is a simple and friendly program to use your
camera.  Use it to take pictures and make videos to share.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kasts
  (package
    (name "kasts")
    (version "25.03.90")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/multimedia/kasts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "094q0yc8ljpkryd0vwwh4ljvk101qr63siwxacm1dgmhyi95262k"))))
    (build-system qt-build-system)
    (native-inputs (list pkg-config extra-cmake-modules))
    (inputs (list bash-minimal
                  breeze-icons
                  gstreamer
                  kcolorscheme
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  ki18n
                  kiconthemes
                  kirigami
                  kirigami-addons
                  kwindowsystem
                  libxkbcommon
                  python
                  qqc2-desktop-style
                  qtdeclarative
                  qtkeychain-qt6
                  qtmultimedia
                  qtsvg
                  sonnet
                  syndication
                  taglib
                  threadweaver
                  vlc
                  vulkan-headers
                  vulkan-loader))
    (arguments
     (list
      #:qtbase qtbase))
    (home-page "https://apps.kde.org/kasts/")
    (synopsis "Convergent podcast client")
    (description
     "Kasts is a convergent podcast application that looks good on
desktop and mobile.

Its main features are:
- Episode management through play queue
- Sync playback positions with other clients through gpodder.net or
  gpodder-nextcloud
- Variable playback speed
- Search for podcasts
- Full system integration: e.g. inhibit system suspend while listening")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public kmix
  (package
    (name "kmix")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmix-" version ".tar.xz"))
       (sha256
        (base32 "1ha0sil2vbpvgys7jkhav7j5g7drg57ypr8c9i3c8ndqwpsyxk3g"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5 pkg-config))
    (inputs
     (list alsa-lib
           glib
           kconfigwidgets-5
           kcompletion-5
           kconfig-5
           kconfigwidgets-5
           kcrash-5
           kdbusaddons-5
           kglobalaccel-5
           ki18n-5
           kiconthemes-5
           knotifications-5
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           libcanberra
           breeze-icons ; default icon set
           plasma-framework
           pulseaudio
           qtbase-5
           solid-5))
    (home-page "https://apps.kde.org/kmix/")
    (synopsis "Volume control and mixer")
    (description "KMix is an audio device mixer, used to adjust volume, select
recording inputs, and set other hardware options.

This package is part of the KDE multimedia module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kwave
  (package
    (name "kwave")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kwave-" version ".tar.xz"))
       (sha256
        (base32 "1g3gaxmchsf9c7zvx608wl41qs001vr1zm0cgnaim753446vb08f"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules (librsvg-for-system) pkg-config kdoctools-5
           tzdata-for-tests))
    (inputs
     (list alsa-lib
           audiofile
           flac
           id3lib
           karchive-5
           kcompletion-5
           kconfig-5
           kconfigwidgets-5
           kcoreaddons-5
           kcrash-5
           kdbusaddons-5
           ki18n-5
           kiconthemes-5
           kio-5
           kservice-5
           ktextwidgets-5
           kwidgetsaddons-5
           kxmlgui-5
           libmad
           libsamplerate
           libvorbis
           opus
           breeze-icons ; default icon set
           pulseaudio
           qtbase-5
           qtmultimedia-5
           zlib))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-TZDATA
            (lambda* (#:key inputs tests? #:allow-other-keys)
              (setenv "TZDIR"
                      (search-input-directory inputs
                                              "share/zoneinfo")))))))
    (home-page "https://apps.kde.org/kwave/")
    (synopsis "Sound editor for KDE")
    (description "Kwave is a sound editor designed for the KDE Desktop
Environment.

With Kwave you can record, play back, import and edit many sorts of audio
files including multi-channel files.  It includes some plugins to transform
audio files in several ways and presents a graphical view with a complete
zoom- and scroll capability.

Its features include:
@itemize
@item 24 Bit Support
@item Undo/Redo
@item Use of multicore CPUs (SMP, hyperthreading)
@item Simple Drag & Drop
@item Realtime Pre-Listen for some effects
@item Support for multi-track files
@item Playback and recording via native ALSA (or OSS, deprecated)
@item Playback via PulseAudio and Phonon
@item Load and edit-capability for large files (can use virtual memory)
@item Reading and auto-repair of damaged wav-files
@item Supports multiple windows
@item Extendable Plugin interface
@item a nice splashscreen
@item some label handling
@end itemize")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+
           license:cc-by-sa3.0 license:cc-by-sa4.0 ;; icons, samples
           license:cc0 license:bsd-3)))) ;; utilities files

(define-public libkcddb
  (package
    (name "libkcddb")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkcddb-" version ".tar.xz"))
       (sha256
        (base32 "0b2khcfm3jnc4iar0ljsq0z3dr3ak6jyaqnbgwj3yk2j05j0yc9n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcodecs
           kconfig
           ki18n
           kio
           kcmutils
           kwidgetsaddons
           libmusicbrainz))
    (arguments
     (list
      #:qtbase qtbase
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
      #:tests? #f)) ; Most tests require network
    (home-page "https://invent.kde.org/multimedia/libkcddb")
    (synopsis "CDDB library for KDE Platform (runtime)")
    (description "A library for retrieving and sending cddb information.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public libkcddb-qt5
  (package
    (inherit libkcddb)
    (name "libkcddb-qt5")
    (native-inputs
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list kcodecs-5
           kconfig-5
           ki18n-5
           kio-5
           kcmutils-5
           kwidgetsaddons-5
           libmusicbrainz))
    (arguments
     (list
      #:qtbase qtbase-5
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=5")
      ;; Most tests require network
      #:tests? #f))))

(define-public libkcompactdisc
  (package
    (name "libkcompactdisc")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkcompactdisc-" version ".tar.xz"))
       (sha256
        (base32 "1lh6vn5aqwlvnb7q29nwxqzb4i4ymd1gs0y1k0vf5czhywrr9gqm"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list alsa-lib
           kcoreaddons
           ki18n
           phonon
           solid))
    (arguments (list
                #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
                #:qtbase qtbase))
    (home-page "https://invent.kde.org/multimedia/libkcompactdisc")
    (synopsis "KDE library for playing & ripping CDs")
    (description "The KDE Compact Disc library provides an API for
applications using the KDE Platform to interface with the CD drives for audio
CDs.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))
