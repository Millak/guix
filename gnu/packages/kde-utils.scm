;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021-2025 Zheng Junjie <z572@z572.online>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages kde-utils)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib) ; dbus for tests
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control))

(define-public ark
  (package
    (name "ark")
    (version "25.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ark-" version ".tar.xz"))
              (sha256
               (base32
                "1h5m9c17ndg02jwgnbxh7fb6xjqy2zivlahbqa15fd4z0h0pw451"))
              ;; The libarchive package in Guix does not support
              ;; xar; disable related tests.
              (patches (search-patches "ark-skip-xar-test.patch"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xserver
            ;; adddialogtest requires DISPLAY.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((xorg-server (assoc-ref inputs "xorg-server")))
                (setenv "HOME" (getcwd))
                (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
                (setenv "DISPLAY" ":1"))))
          (add-after 'install 'wrap-executable
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lrzip (assoc-ref inputs "lrzip"))
                     (lzop  (assoc-ref inputs "lzop"))
                     (p7zip (assoc-ref inputs "p7zip"))
                     (unzip (assoc-ref inputs "unzip"))
                     (zip   (assoc-ref inputs "zip"))
                     (zstd  (assoc-ref inputs "zstd")))
                (wrap-program (string-append out "/bin/ark")
                  `("PATH" suffix
                    ,(map (lambda (p)
                            (string-append p "/bin"))
                          (list lrzip lzop p7zip unzip zip zstd))))))))))
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools xorg-server))
    (inputs
     (list bash-minimal
           breeze-icons
           karchive
           kconfig
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kitemmodels
           kparts
           kpty
           kservice
           kwidgetsaddons
           kfilemetadata
           libarchive
           libzip
           zlib
           ;; Command line tools used by Ark.
           lrzip
           lzop
           p7zip
           unzip
           qtwayland
           zip
           zstd))
    (home-page "https://apps.kde.org/ark/")
    (synopsis "Graphical archiving tool")
    (description "Ark is a graphical file compression/decompression utility
with support for multiple formats, including tar, gzip, bzip2, rar and zip, as
well as CD-ROM images.")
    (license license:gpl2+)))

(define-public atelier
  (let ((commit "8c7f18d3a88b3213546439775a60030b93e1f5b1") ; no releases
        (revision "2"))
    (package
      (name "atelier")
      (version (git-version "0.1-pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://invent.kde.org/utilities/atelier")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vf2j4yjyz21s6bfkzz0jci0h32rnmsm32k1gf6p3i7rlcqx3vyg"))))
      (build-system qt-build-system)
      (arguments (list #:qtbase qtbase #:tests? #f))
      (native-inputs (list extra-cmake-modules qttools pkg-config))
      (inputs (list ki18n
                    kxmlgui
                    kconfigwidgets
                    ktexteditor
                    libatcore
                    qt3d
                    qtcharts
                    qtdeclarative
                    qtmultimedia
                    qtserialport
                    qtwayland))
      (home-page "https://atelier.kde.org")
      (synopsis "Desktop interface to control 3D printers powered by AtCore")
      (description "Atelier provides interface to control and manage your printer.
@itemize
@item Load and see your GCode File
@item Real time graphic to monitor your bed and hotend temperatures
@item You can log everything that comes and go from your printer
@item Edit gcode file
@end itemize")
      (license license:gpl3+))))

(define-public basket
  (let ((commit "bb230be9f6fb838b02ca85c45f9a0ca244efe967")
        (revision "2"))
    (package
      (name "basket")
      (version (git-version "2.49" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/basket")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r5f48vvahhmqfsl9svnkqg83y39570pa944yb9s09h10z96d16i"))))
      (build-system qt-build-system)
      (arguments (list #:qtbase qtbase
                       #:configure-flags
                       #~(list "-DENABLE_GPG=ON"
                               "-DENABLE_GIT=ON")))
      (native-inputs (list extra-cmake-modules
                           kdoctools
                           pkg-config))
      (inputs (list breeze-icons
                    gpgme
                    karchive
                    kcompletion
                    kconfig
                    kconfigwidgets
                    kcoreaddons
                    kcrash
                    kdbusaddons
                    kfilemetadata
                    kglobalaccel
                    kguiaddons
                    ki18n
                    kiconthemes
                    kcmutils
                    kio
                    knotifications
                    kparts
                    kservice
                    ktextwidgets
                    kwidgetsaddons
                    kwindowsystem
                    kxmlgui
                    libgit2
                    phonon
                    qt5compat
                    qtwayland))
      (home-page "https://invent.kde.org/utilities/basket")
      (synopsis "Notes and to-dos organizer")
      (description "This package provides simple note taking and to-do app.")
      (license license:gpl2+))))

(define-public fielding
  (let ((commit "4ee9aea59718851125edcac71e2e4fdc5a592ed9")
        ;; no releases yet
        (revision "2"))
    (package
      (name "fielding")
      (version (git-version "0.1-pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/fielding")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04gzp4bch4k2cvjk0mjcrmjpi986j5bqz3l4xcqykfwbgd08kas2"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules))
      (inputs (list kirigami
                    kirigami-addons
                    kcoreaddons
                    kconfig
                    ki18n
                    kdbusaddons
                    ksyntaxhighlighting
                    qtdeclarative
                    qtsvg
                    qtwayland))
      (arguments (list #:qtbase qtbase
                       #:tests? #f))
      (home-page "https://invent.kde.org/utilities/fielding")
      (synopsis "REST API testing tool")
      (description
       "This package provides a tool for testing REST APIs.")
      (license license:lgpl2.1+))))

(define-public filelight
  (package
    (name "filelight")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/filelight-" version ".tar.xz"))
              (sha256
               (base32
                "1040v9dz2v4ysa68wg3jyv4nmvzsz1dzg1mpsxi3ckqwb9f84fla"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcrash
                  kirigami
                  kirigami-addons
                  kquickcharts
                  kxmlgui
                  kio
                  ki18n
                  kdeclarative
                  qqc2-desktop-style
                  qtsvg
                  qtwayland))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/filelight/")
    (synopsis "Visualize the disk usage")
    (description "Filelight is an application to visualize the disk usage on
your computer.")
    (license license:lgpl2.1+)))

(define-public francis
  (package
    (name "francis")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/francis-" version ".tar.xz"))
              (sha256
               (base32
                "0cgyjmsihxw7ppar9vckpr07w6x7pahhcn0n8phvb3w7aai49gd3"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kirigami
                  kirigami-addons
                  kcoreaddons
                  kconfig
                  ki18n
                  kdbusaddons
                  knotifications
                  qtdeclarative
                  qtsvg
                  qtwayland))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/francis")
    (synopsis "Track your time")
    (description "This package provides time tracking.")
    (license license:lgpl2.1+)))

(define-public isoimagewriter
  (package
    (name "isoimagewriter")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/isoimagewriter-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0bifa4ig9m1scxg34a42xgbzyklqbwmiv3pjyk351mx8i6vv8446"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list gpgme
                  kauth
                  karchive
                  kcoreaddons
                  ki18n
                  kiconthemes
                  kcrash
                  solid
                  kwidgetsaddons
                  qgpgme))
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (home-page "https://invent.kde.org/utilities/isoimagewriter")
    (synopsis "Write hybrid ISO files onto USB disks")
    (description
     "This package provides a tool to write ISO files to USB disks.")
    (license license:gpl3+)))

(define-public kaichat
  (package
    (name "kaichat")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/kaichat/kaichat-" version
                           ".tar.xz"))
       (sha256
        (base32 "18bwannb0p40fpqw0ygiq8m9wfl3k6561l10s7754x7a4lgpfshn"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           knotifications
           knotifyconfig
           kstatusnotifieritem
           ktextaddons
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           purpose
           python
           sonnet))
    (home-page "https://apps.kde.org/kaichat/")
    (synopsis "Chat interface for AI models")
    (description "KAIChat is a chat interface which allows you to chat with AI
models such as Ollama.")
    (license license:gpl3+)))

(define-public kamera
  (package
    (name "kamera")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kamera-" version ".tar.xz"))
              (sha256
               (base32
                "04pdapw7mv311djvv4f7chrj2php8d2nqi3vayv8i4j7zfqbiijd"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcmutils
           kconfig
           kconfigwidgets
           kxmlgui
           ki18n
           kio
           libgphoto2
           qtwayland))
    (home-page "https://apps.kde.org/kamera/")
    (synopsis "KDE integration to digital cameras")
    (description
     "Kamera provides a configuration tool and a KIO worker to read and write
to camera devices supported by @code{libgphoto2} using
@acronym{PTP,Pictute Transfer Protocol}.")
    (license license:gpl2+)))

(define-public kate
  (package
    (name "kate")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kate-" version ".tar.xz"))
       (sha256
        (base32 "02x9zhrk459gz7c9yxql260vgq1s9w5x4ijw3g4mv6qwp2svhbn9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           karchive
           kconfig
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kitemmodels
           threadweaver
           knewstuff
           kio
           kjobwidgets
           kparts
           ktexteditor
           ktextwidgets
           ksyntaxhighlighting
           kwallet
           kwindowsystem
           kxmlgui
           libplasma
           libxkbcommon
           plasma-activities
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; This test requires a 'bin' diretory under '/usr'.
                   (substitute* "addons/externaltools/autotests/externaltooltest.cpp"
                     (("QStringLiteral[(]\"/usr\"[)]")
                      (format #f "QStringLiteral(\"~a\")"
                              (dirname (dirname (which "ls"))))))))
               (add-before 'check 'check-setup
                 (lambda _
                   ;; make Qt render "offscreen", required for tests
                   (setenv "QT_QPA_PLATFORM" "offscreen")
                   (setenv "HOME" (getcwd)))))))
    (home-page "https://kate-editor.org/")
    (synopsis "Multi-document, multi-view text editor")
    (description "Kate is a powerful text editor that can open multiple files
simultaneously.

With a built-in terminal, syntax highlighting, and tabbed sidebar, it performs
as a lightweight but capable development environment.  Kate's many tools,
plugins, and scripts make it highly customizable.

Kate's features include:
@itemize
@item Multiple saved sessions, each with numerous files
@item Scriptable syntax highlighting, indentation, and code-folding
@item Configurable templates and text snippets
@item Symbol viewers for C, C++, and Python
@item XML completion and validation
@end itemize")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0))))

(define-public kdebugsettings
  (package
    (name "kdebugsettings")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdebugsettings-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "181x57rpffg8kz4bfkiwfg65x5dg2qzv6793r0bg2ibch6mhx6bn"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  ki18n
                  kiconthemes
                  kwidgetsaddons
                  kitemviews
                  kcompletion
                  kxmlgui
                  qtwayland))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/kdebugsettings")
    (synopsis "Choose which QLoggingCategory are displayed")
    (description
     "This package selects which QLoggingCategory are displayed.")
    (license license:lgpl2.0+)))

(define-public kbackup
  (package
    (name "kbackup")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kbackup-" version ".tar.xz"))
              (sha256
               (base32
                "097wbyarc0nlshf394c7rp701fsr4m06b0gnlh045fz9rwd1x2gz"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kguiaddons
                  knotifications
                  ki18n
                  kio
                  kxmlgui
                  kiconthemes
                  karchive
                  kwidgetsaddons
                  kstatusnotifieritem
                  libarchive
                  shared-mime-info
                  qt5compat
                  qtwayland))
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (home-page "https://apps.kde.org/kbackup/")
    (synopsis "Backup program with an easy-to-use interface")
    (description
     "This package provides tool to backup your data.
@itemize
@item profile for directories and files to be included or excluded from the
backup
@item The backup target can be either a locally mounted device like a ZIP
drive, USB stick, etc
@item Running automated backups without using a graphical user interface
@end itemize")
    (license license:gpl2+)))

(define-public kcalc
  (package
    (name "kcalc")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcalc-" version ".tar.xz"))
              (sha256
               (base32
                "12l3l7n08kyqf58dngnp3nz8iwkcbhghiihypmd7zr4n0ja2r10f"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list gmp
                  kcoreaddons
                  kcrash
                  kconfig
                  kconfigwidgets
                  kguiaddons
                  ki18n
                  knotifications
                  kxmlgui
                  mpc
                  mpfr
                  qtwayland))
    (home-page "https://apps.kde.org/kcalc/")
    (synopsis "Scientific calculator")
    (description
     "This package provides a scientific calculator.")
    (license license:gpl2+)))

(define-public kcharselect
  (package
    (name "kcharselect")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcharselect-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1qrsvqy73znr5j3qxawj3ljm3yl8l6ir4hpd73hawx83brp27vmg"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs
     (list kbookmarks
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kwidgetsaddons
           kxmlgui
           qtwayland))
    (home-page "https://apps.kde.org/kcharselect/")
    (synopsis "Select and copy special characters from installed fonts")
    (description
     "This package provides a tool to display various information
about the selected character.  This includes not only the Unicode character
name, but also aliases, general notes and cross references to similar
characters.")
    (license license:gpl2+)))

(define-public kdialog
  (package
    (name "kdialog")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdialog-" version ".tar.xz"))
              (sha256
               (base32
                "1d5svwlg7yjv0cf9flfn1zld24lj1dq3xf23aq9jv2mlxhrzrjfj"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list ktextwidgets
                  knotifications
                  kguiaddons
                  kiconthemes
                  kwindowsystem
                  kio
                  kdbusaddons
                  qtwayland))
    (home-page "https://invent.kde.org/utilities/kdialog")
    (synopsis "Show dialog boxes from shell scripts")
    (description "This package provides tool to show nice dialog boxes from
shell scripts.")
    (license license:gpl2+)))

(define-public keurocalc
  (let ((commit "c6e83859624de10210ad6b839c473dd8ea7a0e83") ; just one release
        (revision "2"))
    (package
      (name "keurocalc")
      (version (git-version "1.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/keurocalc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "040x28lnirwply5ph5xz3jcmx7c10qifmwcjgvqymlgqhcfkda0r"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules kdoctools))
      (inputs (list kconfig
                    kconfigwidgets
                    kcoreaddons
                    ki18n
                    kio
                    kwidgetsaddons
                    kxmlgui
                    qtwayland))
      (arguments (list #:qtbase qtbase
                       #:tests? #f))
      (home-page "https://invent.kde.org/utilities/keurocalc")
      (synopsis "Currency conversion tool")
      (description "This package provides a utility to handle currency
conversions between European currencies.")
      (license license:gpl2+))))

(define-public keysmith
  (package
    (name "keysmith")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/keysmith-" version ".tar.xz"))
              (sha256
               (base32
                "098mik8ayiaq1g4x56vwgqadxyyayai69p56ag6vfmq6pvnbr261"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config python-minimal))
    (inputs (list kconfig
                  kcoreaddons
                  kdbusaddons
                  kirigami
                  kirigami-addons
                  ki18n
                  kwindowsystem
                  libsodium
                  openssl
                  prison
                  qqc2-desktop-style
                  qtsvg
                  qtdeclarative
                  qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unrequire-qmlmodule
                 (lambda _
                   ;; HACK: ecm_find_qmlmodule cannot find qmlmodule on other
                   ;; prefix, so we remove its requirement.
                   (substitute* "CMakeLists.txt"
                     (("(org\\.kde\\.prison\\.scanner) REQUIRED" all start)
                      start)))))))
    (home-page "https://invent.kde.org/utilities/keysmith")
    (synopsis "OTP client for Plasma Mobile and Desktop")
    (description
     "This package provides OTP client for Plasma Mobile and Desktop
with support for QR scanning.")
    (license license:gpl3+)))

(define-public kfind
  (package
    (name "kfind")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kfind-" version ".tar.xz"))
              (sha256
               (base32
                "064m1hw6jn0cqlsymf1k0wql3ssz7mhmhgam2796q24jjfc4yvbm"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list karchive
                  kcoreaddons
                  kcrash
                  kfilemetadata
                  kxmlgui
                  ki18n
                  kio
                  kwidgetsaddons
                  qt5compat
                  qtwayland))
    (home-page "https://apps.kde.org/kfind/")
    (synopsis "File search utility")
    (description
     "This package provides a file search utility for KDE.")
    (license license:gpl2+)))

(define-public kirogi
  (let ((commit "73b009f1fc5ac159c2faba720b302c704f89a806") ; no releases yet
        (revision "1"))
    (package
      (name "kirogi")
      (version (git-version "0.1-pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/kirogi")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gncfnwadh11ipynfcrsh1vnk2g02c7scd5wanphi8i95jzak9jd"))))
      (build-system qt-build-system)
      (arguments
       (list #:tests? #f
             #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'fix-gstreamer
                            (lambda* _
                              (substitute* "CMakeLists.txt"
                                (("gstreamer-video-1.0")
                                 "")))))))
      (native-inputs (list extra-cmake-modules pkg-config))
      (inputs (list kconfigwidgets-5
                    kcoreaddons-5
                    ki18n-5
                    kirigami-5
                    kcrash-5
                    kdnssd-5
                    qtquickcontrols2-5
                    qtgraphicaleffects
                    qtdeclarative-5
                    qtgamepad-5
                    qtlocation-5
                    qtwayland-5))
      (propagated-inputs (list gstreamer))
      (home-page "https://apps.kde.org/kirogi/")
      (synopsis "Ground control application for drones")
      (description "Kirogi is a ground control application for drones.
@itemize
@item Direct flight controls
@item Fly by touch on a Navigation Map
@item Trigger vehicle actions (e.g. flips, trim)
@item Gamepad/joypad support
@item Live video
@item Take photo and record video
@item Configure flight parameters (speed, altitude limits)
@item Support for Parrot (Anafi, Bebop 2) and Ryze Tello drones
@end itemize")
      (license ;GPL for programs, LGPL for libraries
       (list license:gpl2+ license:lgpl2.0)))))

(define-public komodo
  (package
    (name "komodo")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/komodo/" version
                           "/komodo-" version ".tar.xz"))
       (sha256
        (base32 "1xdn3k71a5s801p2cpddyvjbpb8ki8i4y2mig15am0v1r2ag16mi"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcolorscheme
           kconfig
           kcoreaddons
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kirigami
           kirigami-addons
           kitemmodels
           qqc2-desktop-style
           qtdeclarative))
    (home-page "https://apps.kde.org/komodo/")
    (synopsis "To-do manager that uses todo.txt")
    (description "KomoDo is a to-do manager that uses
@uref{https://github.com/todotxt/todo.txt/blob/master/README.md, todo.txt
specification}.  It parses any compliant @uref{https://todotxt.org/, todo.txt}
files and turns them into easy to use list of tasks.  It also has built-in help
for the todo.txt specification.

It's features include:
@itemize
@item Open and create new todo.txt files
@item Add, delete and edit tasks
@item Filter and search tasks
@end itemize")
    (license license:gpl2+)))

(define-public kongress
  (package
    (name "kongress")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kongress-" version ".tar.xz"))
              (sha256
               (base32
                "1nsmapfc4fizy9a7cg0pvj32c673qsqfgph331zsclrmjmw5wzwx"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules python-minimal))
    ;; NOTE: Reporting bugs is linked to web browser, better not link it and let
    ;; it reslove through xdg-open in the run time
    (inputs (list kirigami
                  kirigami-addons
                  kcrash
                  kdbusaddons
                  ki18n
                  kcalendarcore
                  kconfigwidgets
                  kwindowsystem
                  kcoreaddons
                  kcontacts
                  kitemmodels
                  knotifications
                  kosmindoormap
                  kxmlgui
                  kiconthemes
                  qtdeclarative
                  qtsvg
                  qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/kongress/")
    (synopsis "Companion application for conferences")
    (description "This application provides list of upcoming conferences with
the schedule and venue information.")
    (license license:gpl3+)))

(define-public kontrast
  (package
    (name "kontrast")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kontrast-" version ".tar.xz"))
              (sha256
               (base32
                "0slwflwafaw0n8zq2qnc3qxnyy4badp6wwl437w2snadza4f02sj"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools python-minimal))
    (inputs (list kcrash
                  kirigami
                  kirigami-addons
                  ki18n
                  kcoreaddons
                  qtdeclarative
                  futuresql
                  qcoro-qt6
                  qtsvg
                  qtwayland))
    (home-page "https://apps.kde.org/kontrast/")
    (synopsis "Color contrast checker")
    (description
     "Kontrast is a color contrast checker and tells you if your color
combinations are distinct enough to be readable and accessible.")
    (license license:gpl3+)))

(define-public libatcore
  (let ((commit "c32a13a90d39e44dc5a8dcb601e2b4aa9c996428")
        (revision "1"))
    (package
      (name "libatcore")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://invent.kde.org/libraries/atcore")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1avcrnxml0iczy0n4xa3ryklbvawbk015wm7l82088qjz2zparcw"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules qttools))
      (inputs (list qtcharts qtdeclarative qtserialport))
      (arguments (list #:qtbase qtbase
                       #:tests? #f))
      (home-page "https://invent.kde.org/libraries/atcore")
      (synopsis "Library for connection and management of 3D printers")
      (description
       "This package provides a API to manage the serial connection between
the computer and 3D Printers.")
      (license (list license:lgpl2.1 license:lgpl3)))))

(define-public kmag
  (package
    (name "kmag")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmag-" version ".tar.xz"))
       (sha256
        (base32 "0s5lm8hxndy78yr4cjf4dazjbhgh1zdap51b2xbld0rvb477jc64"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           ki18n
           kio
           kxmlgui
           breeze-icons ;; default icon set
           libqaccessibilityclient
           qtwayland))
    (home-page "https://apps.kde.org/kmag/")
    (synopsis "Screen magnifier tool")
    (description "You can use KMagnifier to magnify a part of the screen just
as you would use a lens to magnify a newspaper fine-print or a photograph.
This application is useful for a variety of people: from researchers to
artists to web-designers to people with low vision.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmousetool
  (package
    (name "kmousetool")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmousetool-" version ".tar.xz"))
       (sha256
        (base32 "13z04iqf71krpx1p48xapvb3b1y51g8k3dap6l7g2r6g441y1lvr"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           kauth
           kcoreaddons
           kconfigwidgets
           kdbusaddons
           ki18n
           kiconthemes
           knotifications
           kxmlgui
           kwindowsystem
           kstatusnotifieritem
           libxtst
           libxt
           phonon
           qtmultimedia
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/kmousetool/")
    (synopsis "Automatic mouse click and mouse manipulation tool for the
disabled")
    (description "KMouseTool clicks the mouse whenever the mouse cursor pauses
briefly.  It was designed to help those with repetitive strain injuries, for
whom pressing buttons hurts.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmouth
  (package
    (name "kmouth")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmouth-" version ".tar.xz"))
       (sha256
        (base32 "0znkxsqk7fxicd4xkijc4730qy9p5pq9gfy3rbda40ijz676vvrg"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           breeze-icons ;; default icon set
           qtspeech
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/kmouth/")
    (synopsis "Type-and-say frontend for speech synthesizers")
    (description "KMouth is a program which enables persons that cannot speak
to let their computer speak, e.g. mutal people or people who have lost their
voice.  It has a text input field and speaks the sentences that you enter.  It
also has support for user defined phrasebooks.

It includes a history of spoken sentences from which the user can select
sentences to be re-spoken.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public krename
  (let ((commit "33b6d5eec7284d82b9d15c4ea43949b0864a2567")
         (revision "0"))
    (package
      (name "krename")
      (version (git-version "5.0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/krename")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1v056qf4fd1jny0n4n0wka9j0bdx9ii4s0ljb8f2fqgpi2wj57lm"))))
      (build-system qt-build-system)
      (arguments
       (list #:qtbase qtbase
              #:configure-flags
              #~(list "-DQT_MAJOR_VERSION=6")))
      (native-inputs
       (list extra-cmake-modules pkg-config))
      (inputs
       (list exiv2
             freetype
             karchive
             kcompletion
             kconfig
             kcoreaddons
             kcrash
             ki18n
             kiconthemes
             kio
             kitemviews
             kjobwidgets
             kservice
             kwidgetsaddons
             kxmlgui
             podofo
             taglib
             qt5compat
             qtwayland))
      (home-page "https://userbase.kde.org/KRename")
      (synopsis "Utility to handle specialized file renames")
      (description "KRename is a batch file renamer by KDE.  It allows you to
easily rename hundreds or even more files in one go.  The filenames can be
constructed using parts of the original filename or information from the file
metadata such as the creation date or Exif information of an image.

Its features include:

@itemize
@item renaming a list of files based on a set of expressions,
@item copying/moving a list of files to another directory,
@item converting filenames to upper/lower case,
@item adding numbers to filenames,
@item finding and replacing parts of the filename,
@item rename audio files (e.g. mp3, ogg) files based on their metadata,
@item setting access and modification dates, permissions and file ownership,
@item a plug-in API which allows you to extend KRename's features,
@item renaming directories recursively,
@item support for KFilePlugins and
@item creating undo file.
@end itemize")
      (license license:gpl3+))))

(define-public kronometer
  (let ((commit "8cfa062655e89b4b0cad911af3acab6609b13ecd")
        (revision "0"))
    (package
      (name "kronometer")
      (version (git-version "2.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://invent.kde.org/utilities/kronometer.git/")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vzdi9zyrx6s3l74yaxvd1y6729n51m23k0bxydwi36my3zml2kk"))))
      (build-system qt-build-system)
      (native-inputs
       (list extra-cmake-modules kdoctools))
      (inputs
       (list breeze-icons ;; default icon set
             kauth
             kconfig
             kconfigwidgets
             kcoreaddons
             kcrash
             ki18n
             kwidgetsaddons
             kxmlgui
             qtwayland))
      (arguments (list #:qtbase qtbase))
      (home-page "https://apps.kde.org/kronometer/")
      (synopsis "Simple stopwatch application")
      (description "Kronometer is a stopwatch application.  It features the
basic stopwatch actions (pause, resume, reset, laps), as well as the ability
to save the times and resume them later.")
      (license ;; GPL for programs, LGPL for libraries, FDL for documentation
       license:gpl2+))))

(define-public krusader
  (package
    (name "krusader")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/krusader/" version
                           "/krusader-" version ".tar.xz"))
       (sha256
        (base32 "012f75afp7vjpp7wps4lzvcszj6a5y9yzv21wgh9zikcvvx9pdy9"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           karchive
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kglobalaccel
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           knotifications
           kparts
           kstatusnotifieritem
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           qt5compat
           qtwayland
           solid
           zlib))
    (home-page "https://krusader.org/")
    (synopsis "Twin-panel (commander-style) file manager")
    (description "Krusader is a simple, easy, yet powerful,
twin-panel (commander-style) file manager, similar to Midnight Commander or
Total Commander

It provides all the file management features you could possibly want.  Plus:
extensive archive handling, mounted file system support, FTP, advanced search
module, an internal viewer/editor, directory synchronisation, file content
comparisons, powerful batch renaming and much much more.  It supports a wide
variety of archive formats and can handle other KIO slaves such as smb or
fish.

Almost completely customizable, Krusader is very user friendly, fast and looks
great on your desktop.")
    (license license:gpl2+)))

(define-public ktimer
  (package
    (name "ktimer")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ktimer-" version ".tar.xz"))
              (sha256
               (base32
                "0672hpwmvj0378hmf4rqqw1ncynsr8c29dl3r6sdh4qw6kb2dd54"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           kdoctools))
    (inputs
     (list kcrash
           kdbusaddons
           ki18n
           kio
           knotifications
           kconfigwidgets
           kstatusnotifieritem
           qt5compat
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://kde.org/applications/utilities/ktimer")
    (synopsis "Countdown Launcher")
    (description "KTimer is a little tool to execute programs after some time.
It allows you to enter several tasks and to set a timer for each of them.  The
timers for each task can be started, stopped, changed, or looped.")
    (license license:gpl2+)))

(define-public kxstitch
  (let ((commit "bfe934ffc2c2dfa1cc554bc4483a3285b027b00c")
        (revision "0"))
    (package
      (name "kxstitch")
      (version (git-version "2.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://invent.kde.org/graphics/kxstitch.git/")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vhan3pmbm80bh0kdrsmzda2pir3921m8flm1haw8nf5i706gk87"))))
      (build-system qt-build-system)
      (native-inputs
       (list extra-cmake-modules kdoctools pkg-config))
      (inputs
       (list kcompletion
             kconfigwidgets
             kguiaddons
             ki18n
             kio
             ktextwidgets
             kwidgetsaddons
             kxmlgui
             kconfig
             imagemagick
           qtwayland))
      (arguments (list #:qtbase qtbase
                       #:tests? #f))
      (home-page "https://apps.kde.org/kxstitch/")
      (synopsis "Create and print cross stitch patterns")
      (description
       "KXStitch allows creating and printing cross stitch patterns, which can
either be created or generated from a image.")
      (license license:gpl2+))))

(define-public okteta
  (package
    (name "okteta")
    (version "0.26.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/okteta/" version
                           "/src/okteta-" version ".tar.xz"))
       (sha256
        (base32 "0sk5sb0819pl4pfr3rc1as12a2dr9gakbh9z5j3zrm7l08zk5cii"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5 qttools-5 shared-mime-info))
    (inputs
     (list kbookmarks-5
           kcmutils-5
           kcodecs-5
           kcrash-5
           kcompletion-5
           kconfigwidgets-5
           kdbusaddons-5
           ki18n-5
           kiconthemes-5
           kio-5
           knewstuff-5
           kparts-5
           kservice-5
           kwidgetsaddons-5
           kxmlgui-5
           breeze-icons ;; default icon set
           qca
           qtbase-5
           qtdeclarative-5
           qtscript-5
           qtwayland-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "HOME" "/tmp/dummy-home"))))))
    (home-page "https://apps.kde.org/okteta/")
    (synopsis "Hexadecimal editor for binary files")
    (description "Okteta is a simple editor for the raw data of files.  This
type of program is also called hex editor or binary editor.

The data is displayed in the traditional view with two columns: one with the
numeric values and one with the assigned characters.  Editing can be done both
in the value column and the character column.  Besides the usual editing
capabilities Okteta also brings a small set of tools, like a table listing
decodings into common simple data types, a table listing all possible bytes
with its character and value equivalents, a info view with a statistic and a
filter tool.  All modifications to the data loaded can be endlessly undone or
redone.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public rsibreak
  (package
    (name "rsibreak")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/rsibreak/"
                           (version-major+minor version)
                           "/rsibreak-" version ".tar.xz"))
              (sha256
               (base32
                "086ipa9jbpiaj8j79cygk2p5bgpbgpw9bsh8hcbya3vxql4wxcka"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           kcolorscheme
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kidletime
           knotifications
           knotifyconfig
           kstatusnotifieritem
           ktextwidgets
           kwindowsystem
           kxmlgui
           qtwayland))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/rsibreak/")
    (synopsis "Recovery and Repetitive Strain Injury prevention assistsant")
    (description "Repetitive Strain Injury is an illness which can occur as a
result of working with a mouse and keyboard.  This utility can be used to
remind you to take a break now and then.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public sweeper
  (package
    (name "sweeper")
    (version "25.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/sweeper-" version ".tar.xz"))
       (sha256
        (base32 "17b58p83j7gnavmrz7jgi41ssm18z2458wnlwj81zglz3b1vgc3f"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           plasma-activities-stats
           kbookmarks
           kcrash
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           kio
           ktextwidgets
           kxmlgui
           qtwayland))
    (home-page "https://apps.kde.org/sweeper/")
    (synopsis "Temporary file and history cleaner")
    (description "
Sweeper helps to clean unwanted traces the user leaves on the system and to
regain disk space removing unused temporary files.
It can quickly remove temporary information, such as web page cookies,
browser history, or the list of recently-opened documents.  It helps provide
additional privacy on a system shared between multiple users.")
    (license license:lgpl2.0+ )))
