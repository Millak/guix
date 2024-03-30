;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib) ; dbus for tests
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public ark
  (package
    (name "ark")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ark-" version ".tar.xz"))
              (sha256
               (base32
                "081swq9f87yxg4dxdl5i4hszhr0q4ph402in397zfa5vpyspzy41"))
              ;; The libarchive package in Guix does not support
              ;; xar; disable related tests.
              (patches (search-patches "ark-skip-xar-test.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
           khtml
           ki18n
           kiconthemes
           kio
           kitemmodels
           kparts
           kpty
           kservice
           kwidgetsaddons
           libarchive
           libzip
           qtbase-5
           zlib
           ;; Command line tools used by Ark.
           lrzip
           lzop
           p7zip
           unzip
           zip
           zstd))
    (home-page "https://apps.kde.org/ark/")
    (synopsis "Graphical archiving tool")
    (description "Ark is a graphical file compression/decompression utility
with support for multiple formats, including tar, gzip, bzip2, rar and zip, as
well as CD-ROM images.")
    (license license:gpl2+)))

(define-public atelier
  (let ((commit "93d7d440c42f1e49a4933cbbce9f68d5e4ca725a") ; no releases
        (revision "1"))
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
                  "00jccpwvksyp2vr3fjxajs8d9d30rspg4zj6rnj8dai96alp303k"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules pkg-config))
      (inputs (list ki18n
                    kxmlgui
                    kconfigwidgets
                    ktexteditor
                    libatcore
                    qt3d-5
                    qtbase-5
                    qtcharts
                    qtdeclarative-5
                    qtmultimedia-5
                    qtserialport))
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
  (let ((commit "e23a8b3b1198d51f770523c7fb4652750810359a")
        (revision "1"))
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
                  "1i7hrrlwyzzh7mm9xc8hjix24rvy1b2cvvbkhxh9mmdbmphwdhhd"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules))
      (inputs (list breeze-icons
                    karchive
                    kcompletion
                    kconfig
                    kconfigwidgets
                    kcoreaddons
                    kcrash
                    kdbusaddons
                    kdoctools
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
                    phonon))
      (home-page "https://invent.kde.org/utilities/basket")
      (synopsis "Notes and to-dos organizer")
      (description "This package provides simple note taking and to-do app.")
      (license license:gpl2+))))

(define-public fielding
  (let ((commit "6b3c5d67b308e9e7e2043dc6072bfd265ec9f3e1")
        ;; no releases yet
        (revision "1"))
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
                  "1l16am7il7kprmy8irpzj04rb8wbfr84y49wp4i74hspp9xkfick"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules))
      (inputs (list kirigami
                    kcoreaddons
                    kconfig
                    ki18n
                    kdbusaddons
                    ksyntaxhighlighting
                    qtdeclarative-5
                    qtquickcontrols2-5
                    qtsvg-5))
      (home-page "https://invent.kde.org/utilities/fielding")
      (synopsis "REST API testing tool")
      (description
       "This package provides a tool for testing REST APIs.")
      (license license:lgpl2.1+))))

(define-public filelight
  (package
    (name "filelight")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/filelight-" version ".tar.xz"))
              (sha256
               (base32
                "1mwl1dkknvqw9hd5jsh4cdx1zd8f6rxca0vyq01wrx44q9p6dn1n"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kirigami
                  kquickcharts
                  kxmlgui
                  kio
                  ki18n
                  kdeclarative
                  qqc2-desktop-style
                  qtgraphicaleffects
                  qtquickcontrols2-5
                  qtsvg-5))
    (home-page "https://apps.kde.org/filelight/")
    (synopsis "Visualize the disk usage")
    (description "Filelight is an application to visualize the disk usage on
your computer.")
    (license license:lgpl2.1+)))

(define-public francis
  (let ((commit "d2c762ad94170430a667ee57f81ec9dbe498642c") ; no release yet
        (revision "1"))
    (package
      (name "francis")
      (version (git-version "0.1-pre" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/utilities/francis")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "15bk5iq127mp34n9fzq4d5r3qss3ihk93lqy86z2q3lgwid26s0h"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules))
      (inputs (list kirigami
                    kcoreaddons
                    kconfig
                    ki18n
                    kdbusaddons
                    knotifications
                    qtdeclarative-5
                    qtgraphicaleffects
                    qtquickcontrols2-5
                    qtsvg-5))
      (home-page "https://invent.kde.org/utilities/francis")
      (synopsis "Track your time")
      (description "This package provides time tracking.")
      (license license:lgpl2.1+))))

(define-public isoimagewriter
  (package
    (name "isoimagewriter")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/utilities/isoimagewriter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iphp2krgadc175570iiyaxbnjgpc1xilc71gkcbn5n0yd7qmkbv"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kauth
                  karchive
                  kcoreaddons
                  ki18n
                  kiconthemes
                  kcrash
                  solid
                  kwidgetsaddons))
    (home-page "https://invent.kde.org/utilities/isoimagewriter")
    (synopsis "Write hybrid ISO files onto USB disks")
    (description
     "This package provides a tool to write ISO files to USB disks.")
    (license license:gpl3+)))

(define-public kate
  (package
    (name "kate")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kate-" version ".tar.xz"))
       (sha256
        (base32 "0yyhh21pvzsaz7swmghdchzsfk089axhqkjwjv1m8j4q3q3rhv86"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kactivities
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
           ksyntaxhighlighting
           kwallet
           plasma-framework
           kwindowsystem
           kxmlgui
           breeze-icons ;; default icon set
           qtbase-5
           qtscript
           qtx11extras))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdebugsettings-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "02igg8ry1cxa83pdj6pgwzw7hpjwfrfk57d9ybgfvy2x08d5kvqz"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kconfig
                  kdbusaddons
                  ki18n
                  kwidgetsaddons
                  kitemviews
                  kcompletion
                  kxmlgui))
    (home-page "https://invent.kde.org/utilities/kdebugsettings")
    (synopsis "Choose which QLoggingCategory are displayed")
    (description
     "This package selects which QLoggingCategory are displayed.")
    (license license:lgpl2.0+)))

(define-public kbackup
  (package
    (name "kbackup")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kbackup-" version ".tar.xz"))
              (sha256
               (base32
                "121w54ivmq8qnxc97g47i8vq4nkivypp84pqs9rs5bid4cpfvh9p"))))
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
                  libarchive
                  shared-mime-info))
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
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcalc-" version ".tar.xz"))
              (sha256
               (base32
                "04mqicwqn6h99jgh1zl0wsgk1rdkswzxaq8b8yz5hq654dsyq6y1"))))
    (build-system qt-build-system)
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
                  mpfr))
    (home-page "https://apps.kde.org/kcalc/")
    (synopsis "Scientific calculator")
    (description
     "This package provides a scientific calculator.")
    (license license:gpl2+)))

(define-public kcharselect
  (package
    (name "kcharselect")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcharselect-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "05z80j8bwrj1zfpy376gsx30bv7bxsa3lyvrqsz197w1g8vp5gix"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kbookmarks kcoreaddons kcrash ki18n kwidgetsaddons kxmlgui))
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
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdialog-" version ".tar.xz"))
              (sha256
               (base32
                "042az7d9ngar6xp7gv3xcmlns9hpbvs39dkymanqgc0riwa1mvsx"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list ktextwidgets
                  knotifications
                  kguiaddons
                  kiconthemes
                  kwindowsystem
                  kio
                  kdbusaddons))
    (home-page "https://invent.kde.org/utilities/kdialog")
    (synopsis "Show dialog boxes from shell scripts")
    (description "This package provides tool to show nice dialog boxes from
shell scripts.")
    (license license:gpl2+)))

(define-public keurocalc
  (let ((commit "a760d8a7e58b36eb72d15e847f96599c93785194") ; just one release
        (revision "1"))
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
                  "0gh5vwl38hwf1405c980j1fj06g5c52am140lf4mxhrjvnmry7kd"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules kdoctools))
      (inputs (list kconfig
                    kconfigwidgets
                    kcoreaddons
                    ki18n
                    kio
                    kwidgetsaddons
                    kxmlgui))
      (home-page "https://invent.kde.org/utilities/keurocalc")
      (synopsis "Currency conversion tool")
      (description "This package provides a utility to handle currency
conversions between European currencies.")
      (license license:gpl2+))))

(define-public keysmith
  (package
    (name "keysmith")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/keysmith-" version ".tar.xz"))
              (sha256
               (base32
                "1rfp516adliyc57nx4ha1rp8v2z340ygsvblh5sqmsdsg2ivjklj"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kdbusaddons
                  kirigami
                  ki18n
                  kwindowsystem
                  libsodium
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtquickcontrols2-5
                  qtsvg-5))
    (home-page "https://invent.kde.org/utilities/keysmith")
    (synopsis "OTP client for Plasma Mobile and Desktop")
    (description
     "This package provides OTP client for Plasma Mobile and Desktop
with support for QR scanning.")
    (license license:gpl3+)))

(define-public kfind
  (package
    (name "kfind")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kfind-" version ".tar.xz"))
              (sha256
               (base32
                "03g9cn0wp3f2n9zwzbc5sbcria4hcp2ls77fbxyj3wkady3m50if"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list karchive
                  kcoreaddons
                  kfilemetadata
                  ki18n
                  kio
                  kwidgetsaddons))
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
       (list #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'fix-gstreamer
                            (lambda* _
                              (substitute* "CMakeLists.txt"
                                (("gstreamer-video-1.0")
                                 "")))))))
      (native-inputs (list extra-cmake-modules pkg-config))
      (inputs (list kconfigwidgets
                    kcoreaddons
                    ki18n
                    kirigami
                    kcrash
                    kdnssd
                    qtquickcontrols2-5
                    qtgraphicaleffects
                    qtdeclarative-5
                    qtgamepad
                    qtlocation))
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

(define-public kontrast
  (package
    (name "kontrast")
    (version "23.04.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kontrast-" version ".tar.xz"))
              (sha256
               (base32
                "08qwvc2b5bj3012lvwxainbw7d34mkbwwznj3661ydsnfjyxxs92"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list kirigami
                  ki18n
                  kcoreaddons
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtquickcontrols2-5
                  qtsvg-5))
    (home-page "https://apps.kde.org/kontrast/")
    (synopsis "Color contrast checker")
    (description
     "Kontrast is a color contrast checker and tells you if your color
combinations are distinct enough to be readable and accessible.")
    (license license:gpl3+)))

(define-public libatcore
  (let ((commit "0de6393ed3e721537dec50b0ad174d83f1207eb6")
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
                  "1830r6ylpn3l7y2frl8cih5cpjgbkfrib9jq7jklf8aszhlsihf2"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules))
      (inputs (list qtcharts qtdeclarative-5 qtserialport))
      (home-page "https://invent.kde.org/libraries/atcore")
      (synopsis "Library for connection and management of 3D printers")
      (description
       "This package provides a API to manage the serial connection between
the computer and 3D Printers.")
      (license (list license:lgpl2.1 license:lgpl3)))))

(define-public wacomtablet
  (package
    (name "wacomtablet")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/"
                                  name "/" version "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches
                        "wacomtablet-add-missing-includes.patch"
                        "wacomtablet-qt5.15.patch"))
              (sha256
               (base32
                "197pwpl87gqlnza36bp68jvw8ww25znk08acmi8bpz7n84xfc368"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "dbus-launch" "ctest" "-E"
                               "(Test.KDED.DBusTabletService|Test.KDED.TabletHandler|Test.KDED.XInputAdaptor|Test.KDED.XsetWacomAdaptor)")))))))
    (native-inputs (list dbus extra-cmake-modules kdoctools pkg-config))
    (inputs (list kcoreaddons
                  ki18n
                  kglobalaccel
                  kconfig
                  kxmlgui
                  kwidgetsaddons
                  kwindowsystem
                  knotifications
                  kdbusaddons
                  qtx11extras
                  qtdeclarative-5
                  libwacom
                  xf86-input-wacom
                  libxi))
    (propagated-inputs (list plasma-framework))
    (home-page "https://invent.kde.org/system/wacomtablet")
    (synopsis "KDE GUI for the Wacom Linux Drivers")
    (description "Provides KDE GUI for the Wacom Linux Drivers.")
    (license license:gpl2+)))

(define-public kmag
  (package
    (name "kmag")
    (version "23.04.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kmag-" version ".tar.xz"))
      (sha256
       (base32 "13ar37yv3gk5451cdqrgbm91jm50qw4559sx25fv95g2i9wa7z74"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list ki18n
           kio
           kxmlgui
           breeze-icons ;; default icon set
           ;; TODO: QAccessibilityClient - libqaccessibilityclien
           qtbase-5))
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmousetool-" version ".tar.xz"))
       (sha256
        (base32 "1prh9xdzwx0mx93g9cbjy55hxwcci90hvrv2ckj4dqdnv5fv4h21"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kcoreaddons
           kconfigwidgets
           kdbusaddons
           ki18n
           kiconthemes
           knotifications
           kxmlgui
           kwindowsystem
           libxtst
           libxt
           phonon
           breeze-icons ;; default icon set
           qtbase-5))
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmouth-" version ".tar.xz"))
       (sha256
        (base32 "0qyzq4cvcsacb7hr6n79i3rzyjr0m3c8lrf8fwbzdivswpk8wss3"))))
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
           qtbase-5
           qtspeech))
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

(define-public kronometer
  (package
    (name "kronometer")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kronometer/" version
                           "/src/kronometer-" version ".tar.xz"))
       (sha256
        (base32 "0xn4z9y2yl57a5skwp4cjsn1456kiwnwvhrddc0qsihgdyif3fbm"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kwidgetsaddons
           kxmlgui
           breeze-icons ;; default icon set
           qtbase-5))
    (home-page "https://apps.kde.org/kronometer/")
    (synopsis "Simple stopwatch application")
    (description "Kronometer is a stopwatch application.  It features the
basic stopwatch actions (pause, resume, reset, laps), as well as the ability
to save the times and resume them later.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     license:gpl2+)))

(define-public krusader
  (package
    (name "krusader")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/krusader/" version
                           "/krusader-" version ".tar.xz"))
       (sha256
        (base32 "16n2y861ka8jhackf7hd9b0b0argifc1p0a114dvrc0qjddg0k4f"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-compat.h
                 (lambda _
                   ;; Those fallbacks for pre KF-5.91 cause missing includes.
                   (substitute* "app/compat.h"
                     (("#  include <kcompletion_version\\.h>") "")
                     (("#  include <karchive_version\\.h>") "")))))))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kbookmarks
           kcodecs
           kcompletion
           kconfig
           kcoreaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           knotifications
           kparts
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           breeze-icons ;; default icon set
           qtbase-5
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

(define-public kxstitch
  (package
    (name "kxstitch")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kxstitch/" version
                           "/kxstitch-" version ".tar.xz"))
       (sha256
        (base32 "1q6blvcqz6hxdfrkdi0fplmz7rmk3im56kpp68r0yrivhx3hn8sc"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list ktexteditor imagemagick qtbase-5 qtx11extras))
    (home-page "https://apps.kde.org/kxstitch/")
    (synopsis "Create and print cross stitch patterns")
    (description
     "KXStitch allows creating and printing cross stitch patterns, which can
either be created or generated from a image.")
    (license license:gpl2+)))

(define-public okteta
  (package
    (name "okteta")
    (version "0.26.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/okteta/" version
                           "/src/okteta-" version ".tar.xz"))
       (sha256
        (base32 "18bj8gd9kvdk85ypykl668safiyqn5qskgrsb214wxxaprl6phj9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools-5 shared-mime-info))
    (inputs
     (list kbookmarks
           kcmutils
           kcodecs
           kcrash
           kcompletion
           kconfigwidgets
           kdbusaddons
           ki18n
           kiconthemes
           kio
           knewstuff
           kparts
           kservice
           kwidgetsaddons
           kxmlgui
           breeze-icons ;; default icon set
           qca
           qtbase-5
           qtdeclarative-5
           qtscript))
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

(define-public print-manager
  (package
    (name "print-manager")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/print-manager-" version ".tar.xz"))
       (sha256
        (base32 "1fnbkx2xk3pr3cwcji1xbswcf5b7h8r4kag8i3lv28cnjw3ahs52"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list cups
           kcmutils
           kconfig
           kconfigwidgets
           kcoreaddons
           kdbusaddons
           kiconthemes
           ki18n
           kio
           knotifications
           kwidgetsaddons
           kwindowsystem
           plasma-framework
           qtdeclarative-5))
    (home-page "https://invent.kde.org/utilities/print-manager")
    (synopsis "Manage print jobs and printers")
    (description
     "This package provides printing management for KDE.")
    (license license:gpl2+)))

(define-public rsibreak
  (package
    (name "rsibreak")
    (version "0.12.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/rsibreak/0.12/"
                           "rsibreak-" version ".tar.xz"))
       (sha256
        (base32 "0yjv5awngi2hk6xzlwzmj92i6qppnfc0inqdp16rd8gzfpw7xqqw"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kidletime
           knotifications
           knotifyconfig
           ktextwidgets
           kwindowsystem
           kxmlgui
           breeze-icons ;; default icon set
           qtbase-5))
    (home-page "https://apps.kde.org/rsibreak/")
    (synopsis "Assists in the Recovery and Prevention of Repetitive Strain
Injury")
    (description "Repetitive Strain Injury is an illness which can occur as a
result of working with a mouse and keyboard.  This utility can be used to
remind you to take a break now and then.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public smb4k
  (package
    (name "smb4k")
    (version "3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/smb4k/files/"
                           version "/smb4k-" version ".tar.xz"))
       (sha256
        (base32 "0prw0aq16nz9ns4d50mc6fbaw9pbcyh8p698izylhd4i0nr1dd9d"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kconfig
           kconfigwidgets
           kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           kiconthemes
           kio
           kjobwidgets
           knotifications
           knotifyconfig
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           samba
           breeze-icons ;; default icon set
           plasma-framework
           qtbase-5
           qtdeclarative-5
           solid))
    (home-page "https://apps.kde.org/smb4k/")
    (synopsis "Samba (SMB) share advanced browser")
    (description "Smb4K is an network neighborhood browser for the KDE
Software Compilation and a frontend to the programs of the Samba software
suite.

Features:
@itemize
@item Scanning for (active) workgroups, hosts, and shares
@item Support of the CIFS (Linux) and SMBFS (FreeBSD) file system
@item Mounting and unmounting of shares (using the KAuth framework)
@item Access to the files of a mounted share using a file manager or terminal
@item Auto-detection of external mounts and unmounts
@item Remounting of previously used shares on program start
@item Miscellaneous infos about remote network items and mounted shares
@item Network search
@item WINS server support
@item Preview of the contents of a share
@item Several methods to look up the initial list of workgroups and domains
@item Default login
@item Special handling of homes shares
@item Ability to bookmark favorite shares and organize them in groups
@item System tray widget
@item Support of advanced Samba options
@item Support of printer shares
@item KWallet support
@item Synchronization of a remote share with a local copy and vice versa
@item Ability to define custom options for individual servers and shares
@item Laptop support through the Solid hardware device framework
@end itemize")
    (license license:gpl2+)))

(define-public sweeper
  (package
    (name "sweeper")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/sweeper-" version ".tar.xz"))
       (sha256
        (base32 "19b382cgdcd4qh4ppdmbhsacvcc9nlbs7spcg8ii02bdpx6qw9b7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kactivities-stats
           kbookmarks
           kcrash
           kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           kio
           ktextwidgets
           kxmlgui
           breeze-icons ;; default icon set
           qtbase-5))
    (home-page "https://apps.kde.org/sweeper/")
    (synopsis "Temporary file and history cleaner")
    (description "
Sweeper helps to clean unwanted traces the user leaves on the system and to
regain disk space removing unused temporary files.
It can quickly remove temporary information, such as web page cookies,
browser history, or the list of recently-opened documents.  It helps provide
additional privacy on a system shared between multiple users.")
    (license license:lgpl2.0+ )))
