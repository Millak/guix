;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2021, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023, 2025 Zheng Junjie <z572@z572.online>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gpodder)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public phonon
  (package
    (name "phonon")
    (version "4.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/phonon"
                    "/" version "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "16pk8g5rx00x45gnxrqg160b1l02fds1b7iz6shllbfczghgz1rj"))))
    (build-system cmake-build-system)
    (native-inputs
     (list appstream extra-cmake-modules pkg-config qttools))
    (inputs (list qtbase qt5compat glib qtbase-5 pulseaudio))
    (arguments
     (list #:configure-flags
           #~(list "-DCMAKE_CXX_FLAGS=-fPIC")))
    (home-page "https://community.kde.org/Phonon")
    (synopsis "KDE's multimedia library")
    (description "KDE's multimedia library.")
    (license license:lgpl2.1+)))

(define-public phonon-backend-gstreamer
  (package
    (name "phonon-backend-gstreamer")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/phonon/"
                    name "/" version "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1wk1ip2w7fkh65zk6rilj314dna0hgsv2xhjmpr5w08xa8sii1y5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list phonon
           qtbase-5
           qtx11extras
           gstreamer
           gst-plugins-base
           libxml2))
    (arguments
     `(#:tests? #f
       #:configure-flags
       '( "-DPHONON_BUILD_PHONON4QT5=ON")))
    (home-page "https://community.kde.org/Phonon")
    (synopsis "Phonon backend which uses GStreamer")
    (description "Phonon makes use of backend libraries to provide sound.
Phonon-GStreamer is a backend based on the GStreamer multimedia library.")
    ;; license: source files mention "either version 2.1 or 3"
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public audiocd-kio
  (package
    (name "audiocd-kio")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/audiocd-kio-" version ".tar.xz"))
       (sha256
        (base32 "0w60xh54cbgjw6f3lqjgwx35knwbin0zv9crsh2qf8jdzzgw2av7"))))
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
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
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

(define-public amarok
  (package
    (name "amarok")
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/amarok/" version
                                  "/amarok-" version ".tar.xz"))
              (sha256
               (base32
                "00cw6gk1vhc5ch2jri90lma5jbkah3bq1dmyzg49bnq77aljwvrr"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DBUILD_WITH_QT6=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'check-setup
                 (lambda _
                   ;; Set home directory.
                   (setenv "HOME" "/tmp")
                   ;; testplaylistlayout looks for "amarok/data" directory in
                   ;; $XDG_DATA_DIRS. Maybe it is for testing after installing.
                   ;; As a workaround, set XDG_DATA_DIRS pointing to $TMPDIR
                   ;; which contains "amarok/data" directory.
                   (let ((linktarget (string-append (dirname (getcwd))
                                                    "/amarok")))
                     (if (not (equal? (basename (getcwd)) "amarok"))
                       (symlink (getcwd) linktarget))
                     (setenv "XDG_DATA_DIRS"
                             (string-append (getenv "XDG_DATA_DIRS") ":"
                                            (dirname linktarget))))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; testsqlscanmanager fails, even when run manually.
                     (invoke "ctest" "-E" "testsqlscanmanager")))))))
    (native-inputs
     (list extra-cmake-modules
           googletest
           kdoctools
           `(,mariadb-embedded "dev")
           pkg-config
           qttools))
    (inputs
     ;; TODO: Add packages containing "gstreamer-cdda-1.0" and
     ;; "gstreamer-netbuffer-1.0" modules.
     (list ffmpeg
           fftw
           glib
           gstreamer
           gst-plugins-bad
           gst-plugins-base
           gst-plugins-good
           gst-plugins-ugly
           gst-libav
           karchive
           kcodecs
           kcolorscheme
           kconfig
           kconfigwidgets
           kcoreaddons
           kcmutils
           kcrash
           kdbusaddons
           kdnssd
           kglobalaccel
           kguiaddons
           ki18n
           kiconthemes
           kio
           kirigami
           knotifications
           kpackage
           kstatusnotifieritem
           ktexteditor
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           libofa
           libmtp
           libmygpo-qt
           libxcrypt
           `(,mariadb-embedded "lib")
           openssl
           python
           qt5compat
           qtsvg
           qtwayland
           qtwebengine
           solid
           taglib
           threadweaver))
    (home-page "https://amarok.kde.org/")
    (synopsis "Audio player for KDE")
    (description
     "Amarok is a music player and collection manager.  It features:
@itemize
@item dynamic playlists matching different criteria,
@item collection managing with rating support,
@item support for basic MTP and UMS music player devices,
@item integrated internet services such as Magnatune, Ampache and more,
@item scripting support,
@item cover manager and
@item replay gain support
@end itemize")
    (license license:gpl2+)))

(define-public dragon
  (package
    (name "dragon")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dragon-" version ".tar.xz"))
       (sha256
        (base32 "07vpbxfxrawl9ybg48zcc0rb99pj630wqzf7mg76i5qyilckwvxz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list ffmpeg
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
           kirigami
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           breeze-icons ; default icon set
           qtmultimedia
           solid))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
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
           #:tests? #f
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/elisa-" version ".tar.xz"))
       (sha256
        (base32 "1s14gxfiq51zchmi1xm89237i3gmgrkjkkwhagll6wxq1kfixfkb"))))
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
     (list #:qtbase qtbase
           #:tests? #f ;; many tests fail
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'start-xorg-server
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; The test suite requires a running X server, setting
                   ;; QT_QPA_PLATFORM=offscreen does not suffice.
                   (system "Xvfb :1 -screen 0 640x480x24 &")
                   (setenv "DISPLAY" ":1")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                     (invoke "dbus-launch" "make" "test")))))))
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ffmpegthumbs-" version ".tar.xz"))
       (sha256
        (base32 "1pyvjggjrhzvbiyafba9pcxbixg74ip6clbzr1wm2n4lcf1j6c1c"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list ffmpeg kconfig ki18n kio taglib))
    (arguments (list #:qtbase qtbase
                     #:tests? #f
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/juk-" version ".tar.xz"))
       (sha256
        (base32 "1mwdsyqcvwiz7lcq9l6sywqnbsc7916racgd1sgrwwswmdf4p8ir"))))
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
           qtbase
           qtmultimedia
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

(define-public kdenlive
  (package
    (name "kdenlive")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdenlive-" version ".tar.xz"))
       (sha256
        (base32 "1ysp86iq69mb08cxpp8vqqf19kdgkw4dj1y08bzi15dk1ll6vaac"))))
    (build-system qt-build-system)
    (arguments
     ;; XXX otiotest seemingly freezes.  Additionally, tests/mixtest.cpp:818
     ;; fails with an unexpected exception.
     (list
      #:qtbase qtbase
      #:configure-flags #~(list "-DFETCH_OTIO=off")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-executable
            (lambda _
              (let* ((ffmpeg #$(this-package-input "ffmpeg"))
                     (frei0r #$(this-package-input "frei0r-plugins"))
                     (ladspa #$(this-package-input "ladspa"))
                     (qtbase #$(this-package-input "qtbase")))
                (wrap-program (string-append #$output "/bin/kdenlive")
                  `("PATH" ":" prefix
                    ,(list (string-append ffmpeg "/bin")))
                  `("FREI0R_PATH" ":" =
                    (,(string-append frei0r "/lib/frei0r-1")))
                  `("LADSPA_PATH" ":" =
                    (,(string-append ladspa "/lib/ladspa")))
                  `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                    (,(string-append qtbase "/lib/qt6/plugins/platforms")))
                  `("MLT_PREFIX" ":" =
                    (,#$(this-package-input "mlt"))))))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config qttools))
    (inputs
     (list bash-minimal
           breeze                       ; make dark theme available easily
           breeze-icons                 ; recommended icon set
           ffmpeg
           frei0r-plugins
           imath
           karchive
           kcrash
           kdbusaddons
           kdeclarative
           kdoctools
           kfilemetadata
           kguiaddons
           kiconthemes
           kirigami
           knewstuff
           knotifications
           knotifyconfig
           kparts
           kplotting
           ktextwidgets
           ladspa
           mlt
           opentimelineio
           purpose
           qqc2-desktop-style
           qtbase
           qtdeclarative
           qtmultimedia
           qtnetworkauth
           qtsvg
           shared-mime-info))
    (home-page "https://kdenlive.org")
    (synopsis "Non-linear video editor")
    (description "Kdenlive is an acronym for KDE Non-Linear Video Editor.

Non-linear video editing is much more powerful than beginner's (linear)
editors, hence it requires a bit more organization before starting.  However,
it is not reserved to specialists and can be used for small personal
projects.")
    (license license:gpl2+)))

(define-public kid3
  (package
    (name "kid3")
    (version "3.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kid3/" version
                           "/kid3-" version ".tar.xz"))
       (sha256
        (base32 "0q07f4fwh8lwbqi7qm2ga01a6hsqaarnr1vqi6npipnxskvyxkzr"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-DBUILD_WITH_QT6=ON"
         "-DWITH_FFMPEG=ON"
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
           kdoctools
           libxslt
           python-minimal-wrapper
           qttools))
    (inputs
     (list chromaprint
           ffmpeg-6
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/k3b-" version ".tar.xz"))
       (sha256
        (base32 "0dhmfbbpznf1axix1npnx46m3wqs1lxcjj167k9il2jz49bf1k47"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
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
     (list extra-cmake-modules pkg-config kdoctools))
    (inputs
     (list bash-minimal
           cdrdao
           cdrtools
           dvd+rw-tools
           ffmpeg-6
           flac
           karchive
           kauth
           kcmutils
           kconfig
           kcoreaddons
           kfilemetadata
           ki18n
           kiconthemes
           kio
           kjobwidgets
           knewstuff
           knotifications
           knotifyconfig
           kservice
           kwidgetsaddons
           kxmlgui
           lame
           libburn
           libcdio-paranoia
           libdvdcss
           libdvdread
           ;; TODO: LibFuzzer
           libkcddb
           libmad
           libmpcdec
           ;;("libmusicbrainz" ,libmusicbrainz) ; wants old version 2
           libsamplerate
           libsndfile
           libvorbis
           breeze-icons ; default icon set
           shared-mime-info
           solid
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
  (let* ((commit "0a363690f5b320ec55f190a4d32d09d73a8c86f1")
         (revision "0"))
    (package
      (name "kaffeine")
      (version (git-version "2.0.19" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://invent.kde.org/multimedia/kaffeine.git/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0959sw7xdkv3blv6k1p7k91ki0s30ki54jh516n52lp6h48q6z1p"))))
      (build-system qt-build-system)
      (native-inputs
       (list extra-cmake-modules pkg-config kdoctools))
      (inputs
       (list eudev
             kcoreaddons
             kdbusaddons
             ki18n
             kio
             kwidgetsaddons
             kwindowsystem
             kxmlgui
             libxscrnsaver
             breeze-icons ; default icon set
             solid
             v4l-utils ; libdvbv5
             vlc))
      (arguments
       (list #:qtbase qtbase
             #:tests? #f
             #:phases
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
       (list license:gpl2+ license:fdl1.2+)))))

(define-public kamoso
  (package
    (name "kamoso")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kamoso-" version ".tar.xz"))
       (sha256
        (base32 "0fsvmrsnbac9nsqds53zzrzanq776fn1zs45ihc9p6kq26qpnd65"))))
    (build-system qt-build-system)
    (native-inputs
     (list
      extra-cmake-modules
      `(,glib "bin")
      kdoctools
      pkg-config))
    (inputs
     (list gstreamer
           gst-plugins-base
           kconfig
           ki18n
           kio
           kirigami
           knotifications
           kparts
           breeze-icons ; default icon set
           purpose
           qtdeclarative))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/kamoso/")
    (synopsis "Take pictures and videos out of your webcam")
    (description "Kamoso is a simple and friendly program to use your
camera.  Use it to take pictures and make videos to share.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kasts
  (package
    (name "kasts")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kasts-" version ".tar.xz"))
       (sha256
        (base32 "0fihb3kyml2asa96glz28vkiay2cjirrqy4py9vgz4jrb627y76j"))))
    (build-system qt-build-system)
    (native-inputs (list pkg-config extra-cmake-modules))
    (inputs (list breeze-icons
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
      #:qtbase qtbase
      #:tests? #f))
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmix-" version ".tar.xz"))
       (sha256
        (base32 "0046rmgi7bwy3chmhd7g9snj0y1fb5zhxydn9sz73ha4nfp6pvd3"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list alsa-lib
           glib
           kcompletion
           kconfig
           kconfigwidgets
           kcrash
           kdbusaddons
           kglobalaccel
           ki18n
           kiconthemes
           knotifications
           kstatusnotifieritem
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libcanberra
           breeze-icons ; default icon set
           libplasma
           pulseaudio
           solid))
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kwave-" version ".tar.xz"))
       (sha256
        (base32 "1cbw2f3yrm8iywjpj4873qxcbqgpzbc1np4v28cjvyxcna4yxyib"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules (librsvg-for-system) pkg-config kdoctools
           tzdata-for-tests))
    (inputs
     (list alsa-lib
           audiofile
           flac
           id3lib
           karchive
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kservice
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libmad
           libsamplerate
           libvorbis
           opus
           breeze-icons ; default icon set
           pulseaudio
           qtmultimedia
           zlib))
    (arguments
     (list
      #:qtbase qtbase
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
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkcddb-" version ".tar.xz"))
       (sha256
        (base32 "0vwd6cnfiwwx2kd32dqn1k3fk9csghpmqyllnq786fp5ik8f8hkh"))))
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

(define-public libkcompactdisc
  (package
    (name "libkcompactdisc")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkcompactdisc-" version ".tar.xz"))
       (sha256
        (base32 "0718v5yff3saqqxlpqh68lpc64bq7dk0qlagkprgyv8h69bz9fff"))))
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
                #:tests? #f
                #:qtbase qtbase))
    (home-page "https://invent.kde.org/multimedia/libkcompactdisc")
    (synopsis "KDE library for playing & ripping CDs")
    (description "The KDE Compact Disc library provides an API for
applications using the KDE Platform to interface with the CD drives for audio
CDs.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public mpvqt
  (package
    (name "mpvqt")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde//stable/mpvqt/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cix3ssvpw9wg8h06zr5x0jcm1f1p0c6524ac9zh3wwc6dlymldx"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list qtdeclarative))
    (propagated-inputs
     (list mpv))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://invent.kde.org/libraries/mpvqt")
    (synopsis "libmpv wrapper for QtQuick2 and QML")
    (description "This package provides a libmpv wrapper for QtQuick2 and QML.")
    (license license:lgpl2.1+)))

(define-public plasmatube
  (package
    (name "plasmatube")
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/plasmatube-" version ".tar.xz"))
              (sha256
               (base32
                "0bqd01qkc063jbcfdhn5mfq631hn9gpa7nkik749c457g3763b6s"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config python-minimal))
    (inputs
     (list kconfig
           kcoreaddons
           kdbusaddons
           kirigami
           kirigami-addons
           ki18n
           kwindowsystem
           mpvqt
           purpose
           qtdeclarative
           qtmultimedia
           qtsvg
           qtkeychain-qt6
           qtwayland
           yt-dlp))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/plasmatube/")
    (synopsis "Kirigami YouTube video player")
    (description "This package provides YouTube video player based
on QtMultimedia and @command{yt-dlp}.")
    (license license:gpl3+)))
