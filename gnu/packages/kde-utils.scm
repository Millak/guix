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
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public ark
  (package
    (name "ark")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/ark-" version ".tar.xz"))
              (sha256
               (base32
                "1f96k7jisjdvpg8wjrm1pyz6p6zkqcx70054cg3sd0244skrzrna"))
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
      (inputs (list ki18n-5
                    kxmlgui-5
                    kconfigwidgets-5
                    ktexteditor-5
                    libatcore
                    qt3d-5
                    qtbase-5
                    qtcharts-5
                    qtdeclarative-5
                    qtmultimedia-5
                    qtserialport-5))
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
                    qtsvg))
      (arguments (list #:qtbase qtbase))
      (home-page "https://invent.kde.org/utilities/fielding")
      (synopsis "REST API testing tool")
      (description
       "This package provides a tool for testing REST APIs.")
      (license license:lgpl2.1+))))

(define-public filelight
  (package
    (name "filelight")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/filelight-" version ".tar.xz"))
              (sha256
               (base32
                "0mibgqhab7cbis0vw89f5y73185jq1dgmcsqdd8g4clkglgvdlrk"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kirigami
                  kirigami-addons
                  kquickcharts
                  kxmlgui
                  kio
                  ki18n
                  kdeclarative
                  qqc2-desktop-style
                  qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/filelight/")
    (synopsis "Visualize the disk usage")
    (description "Filelight is an application to visualize the disk usage on
your computer.")
    (license license:lgpl2.1+)))

(define-public francis
  (package
    (name "francis")
    (version "24.05.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/utilities/francis")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lm0gh035qc9ip9hs00gxl3y9d9a8846ggypzgzngkqazic9i82k"))))
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
                  qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/francis")
    (synopsis "Track your time")
    (description "This package provides time tracking.")
    (license license:lgpl2.1+)))

(define-public isoimagewriter
  (package
    (name "isoimagewriter")
    (version "24.05.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/utilities/isoimagewriter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v8ggdgij503m3bm1w24840al5by7y62p8yrmhlk4g0hjacmkys7"))))
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
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/isoimagewriter")
    (synopsis "Write hybrid ISO files onto USB disks")
    (description
     "This package provides a tool to write ISO files to USB disks.")
    (license license:gpl3+)))

(define-public kate
  (package
    (name "kate")
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kate-" version ".tar.xz"))
       (sha256
        (base32 "085hbl6xzzihnhy8pjwdjdsrww6l1h70m4sf2s1b5c1xsnvhhkvp"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           plasma-activities
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
           libxkbcommon))
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
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdebugsettings-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1il9jcwya1hgqrs5yrlahk9h0hrrs5h1ay304sbixgiiafx1dcv7"))))
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
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/kdebugsettings")
    (synopsis "Choose which QLoggingCategory are displayed")
    (description
     "This package selects which QLoggingCategory are displayed.")
    (license license:lgpl2.0+)))

(define-public kbackup
  (package
    (name "kbackup")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kbackup-" version ".tar.xz"))
              (sha256
               (base32
                "0s75il0hxs95sdmj3jll8rdl1n8y86qgwww15idda18yww8d0bwm"))))
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
                  qt5compat))
    (arguments (list #:qtbase qtbase))
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
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcalc-" version ".tar.xz"))
              (sha256
               (base32
                "0p1m3yv52dc2mzalk19l3zpdwpwi5jg0fib5lgb1ln71kwb07y8n"))))
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
                  mpfr))
    (home-page "https://apps.kde.org/kcalc/")
    (synopsis "Scientific calculator")
    (description
     "This package provides a scientific calculator.")
    (license license:gpl2+)))

(define-public kcharselect
  (package
    (name "kcharselect")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kcharselect-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0yzw3jqcxkqi3ricsxcj1g8lhwxqjx0vwa91h4jb7ig1gsp6h3hi"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdialog-" version ".tar.xz"))
              (sha256
               (base32
                "06h42dlchikn0xva93yj2y46gmcqb78lm0qim30q25fnz5hbfv5f"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
                    kxmlgui))
      (arguments (list #:qtbase qtbase))
      (home-page "https://invent.kde.org/utilities/keurocalc")
      (synopsis "Currency conversion tool")
      (description "This package provides a utility to handle currency
conversions between European currencies.")
      (license license:gpl2+))))

(define-public keysmith
  (package
    (name "keysmith")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/keysmith-" version ".tar.xz"))
              (sha256
               (base32
                "1n34sda27hpl53gayglrjyz8la8g25z8mrvaymwhcp8fzpids911"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config python-minimal))
    (inputs (list kdbusaddons
                  kirigami
                  ki18n
                  kwindowsystem
                  libsodium
                  qqc2-desktop-style
                  qtsvg
                  qtdeclarative))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/utilities/keysmith")
    (synopsis "OTP client for Plasma Mobile and Desktop")
    (description
     "This package provides OTP client for Plasma Mobile and Desktop
with support for QR scanning.")
    (license license:gpl3+)))

(define-public kfind
  (package
    (name "kfind")
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kfind-" version ".tar.xz"))
              (sha256
               (base32
                "18r0fkv5dnl1l23c94igf09g71z3pk571hh1ff4df9ixajyvw43b"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list karchive
                  kcoreaddons
                  kfilemetadata
                  kxmlgui
                  ki18n
                  kio
                  kwidgetsaddons
                  qt5compat))
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
      (inputs (list kconfigwidgets-5
                    kcoreaddons-5
                    ki18n-5
                    kirigami-5
                    kcrash-5
                    kdnssd-5
                    qtquickcontrols2-5
                    qtgraphicaleffects
                    qtdeclarative-5
                    qtgamepad
                    qtlocation-5))
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
    (version "24.05.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kontrast-" version ".tar.xz"))
              (sha256
               (base32
                "0rn7nw686d773hx6dg1zvld89r0l9gg4fps3ipyhlwi8gwcm1dwl"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules kdoctools python-minimal))
    (inputs (list kirigami
                  ki18n
                  kcoreaddons
                  qtdeclarative
                  futuresql
                  qcoro-qt6
                  qtsvg))
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
      (inputs (list qtcharts-5 qtdeclarative-5 qtserialport-5))
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
    (native-inputs (list dbus extra-cmake-modules kdoctools-5 pkg-config))
    (inputs (list kcoreaddons-5
                  ki18n-5
                  kglobalaccel-5
                  kconfig-5
                  kxmlgui-5
                  kwidgetsaddons-5
                  kwindowsystem-5
                  knotifications-5
                  kdbusaddons-5
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmag-" version ".tar.xz"))
       (sha256
        (base32 "0sbm9jr0spywgvm136swgbdqrw23dsf2msbj8x9sv4j004nx7sww"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list ki18n
           kio
           kxmlgui
           breeze-icons ;; default icon set
           libqaccessibilityclient))
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmousetool-" version ".tar.xz"))
       (sha256
        (base32 "1vhabwsi1iiliakyqdaxjlzx9n7ln08szrr1l6hyi7abvzaarp8p"))))
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
           qtmultimedia))
    (arguments (list #:qtbase qtbase))
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmouth-" version ".tar.xz"))
       (sha256
        (base32 "1gd51vwa9xdxxi6idzgzfb9dapirp5xc9alf3d9xnliwxqfxiv3x"))))
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
           qtspeech))
    (arguments (list #:qtbase qtbase))
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
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list kauth-5
           kconfig-5
           kconfigwidgets-5
           kcoreaddons-5
           kcrash-5
           ki18n-5
           kwidgetsaddons-5
           kxmlgui-5
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
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/krusader/" version
                           "/krusader-" version ".tar.xz"))
       (sha256
        (base32 "10w2y66kapd0ii8lr37jjwgg2a623ds05bv416j1payh352j1grp"))))
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
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list karchive-5
           kbookmarks-5
           kcodecs-5
           kcompletion-5
           kconfig-5
           kcoreaddons-5
           kguiaddons-5
           ki18n-5
           kiconthemes-5
           kio-5
           kitemviews-5
           knotifications-5
           kparts-5
           ktextwidgets-5
           kwallet-5
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           breeze-icons ;; default icon set
           qtbase-5
           solid-5
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
     (list extra-cmake-modules kdoctools-5 pkg-config))
    (inputs
     (list ktexteditor-5 imagemagick qtbase-5 qtx11extras))
    (home-page "https://apps.kde.org/kxstitch/")
    (synopsis "Create and print cross stitch patterns")
    (description
     "KXStitch allows creating and printing cross stitch patterns, which can
either be created or generated from a image.")
    (license license:gpl2+)))

(define-public okteta
  (package
    (name "okteta")
    (version "0.26.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/okteta/" version
                           "/src/okteta-" version ".tar.xz"))
       (sha256
        (base32 "0qvwryk8m2ixxn9f6xgjiy3nv0vffapkgx8qw9315306yw750cq5"))))
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

(define-public rsibreak
  (package
    (name "rsibreak")
    (version "0.12.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/rsibreak/0.12/"
                           "rsibreak-" version ".tar.xz"))
       (sha256
        (base32 "0kfbbhyzilvar3vns68pd8vkd17f07g8q9g83xxwl06zl3k6672j"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list kauth-5
           kconfig-5
           kconfigwidgets-5
           kcoreaddons-5
           kcrash-5
           kdbusaddons-5
           ki18n-5
           kiconthemes-5
           kidletime-5
           knotifications-5
           knotifyconfig-5
           ktextwidgets-5
           kwindowsystem-5
           kxmlgui-5
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
    (version "3.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/smb4k/files/"
                           version "/smb4k-" version ".tar.xz"))
       (sha256
        (base32 "1d53yl02wrfl6wl2h4a30qiirs44qmfkfsr1kjv69fqhbqp2cszs"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools-5))
    (inputs
     (list kauth-5
           kconfig-5
           kconfigwidgets-5
           kcompletion-5
           kcoreaddons-5
           kcrash-5
           kdbusaddons-5
           kdnssd-5
           ki18n-5
           kiconthemes-5
           kio-5
           kjobwidgets-5
           knotifications-5
           knotifyconfig-5
           ktextwidgets-5
           kwallet-5
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           samba
           breeze-icons ;; default icon set
           plasma-framework
           qtbase-5
           qtdeclarative-5
           solid-5))
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
    (version "24.05.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/sweeper-" version ".tar.xz"))
       (sha256
        (base32 "0s1cj86p8wa9ngdqxff6mf4fx4b388b6il02g442g8q1kcqcnla5"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
           kxmlgui))
    (home-page "https://apps.kde.org/sweeper/")
    (synopsis "Temporary file and history cleaner")
    (description "
Sweeper helps to clean unwanted traces the user leaves on the system and to
regain disk space removing unused temporary files.
It can quickly remove temporary information, such as web page cookies,
browser history, or the list of recently-opened documents.  It helps provide
additional privacy on a system shared between multiple users.")
    (license license:lgpl2.0+ )))
