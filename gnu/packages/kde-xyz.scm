;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sughosha <sughosha@disroot.org>
;;; Copyright © 2026 nick <nicholascaitong@gmail.com>
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

(define-module (gnu packages kde-xyz)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public kde-material-you-colors
  (package
    (name "kde-material-you-colors")
    ;; This commit fixes loading the widget on Qt version 6.9 or older.
    (properties '((commit . "72421299789ce863d902dec38daaadaddc378b2e")
                  (revision . "0")))
    (version (git-version "2.2.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/luisbocanegra/kde-material-you-colors")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kp9ri500pn3x3pwjdik0ciqqpdk8yy749ldmasxjwfvk9ydzjjd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: stty: 'standard input': Inappropriate ioctl for device
          (delete 'sanity-check)
          (add-after 'unpack 'fix-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/kde_material_you_colors/utils/utils.py"
                (("\"whereis")
                 (string-append "\""
                                (search-input-file inputs "/bin/whereis")))
                (("\"stty")
                 (string-append "\""
                                (search-input-file inputs "/bin/stty"))))
              (substitute* "src/kde_material_you_colors/utils/kwin_utils.py"
                (("\"gdbus")
                 (string-append "\""
                                (search-input-file inputs "/bin/gdbus")))))))))
    (native-inputs (list python-setuptools))
    (inputs (list coreutils-minimal       ;for stty
                  `(,glib "bin")          ;for gdbus
                  python-dbus
                  python-materialyoucolor
                  python-numpy
                  python-pillow
                  python-magic
                  python-pywal16          ;optional
                  util-linux))            ;for whereis
    (home-page "https://github.com/luisbocanegra/kde-material-you-colors")
    (synopsis "Automatic Material You colors generator for the KDE Plasma")
    (description
     "KDE Material You Colors is an automatic Material You colors generator
from your wallpaper for KDE Plasma.")
    (license license:gpl3)))

(define-public kwin-effects-better-blur-dx
  (package
    (name "kwin-effects-better-blur-dx")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/xarblu/kwin-effects-better-blur-dx")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11rn887ll3p3cnjbqlvwnaqb0drfc1a6h4f224j7wk0dzq6ymqvw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;no tests
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcmutils
           kcolorscheme
           kconfig
           kcoreaddons
           kdecoration
           ki18n
           kwidgetsaddons
           kwin
           kwindowsystem
           libepoxy
           libx11
           libxcb
           qtbase
           qtdeclarative
           wayland))
    (home-page "https://github.com/xarblu/kwin-effects-better-blur-dx")
    (synopsis "Fork of the KDE Plasma 6 blur effect")
    (description
     "Better Blur DX is a fork of the KDE Plasma 6 blur effect with additional
features and bug fixes that haven't been patched yet.

Features:
@itemize
@item Force blur
@item Adjustable blur brightness, contrast and satuarion
@item Adjustable corner radius
@item Refraction
@end itemize

Bug fixes:
@itemize
@item Fix for blur that may sometimes disappear during animations
@item Fix for transparent color schemes not working properly with the Breeze
 application style
@end itemize")
    (license license:gpl2+)))

(define-public kwin-effects-rounded-corners
  (package
    (name "kwin-effects-rounded-corners")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/matinlotfali/KDE-Rounded-Corners")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03d13vg7qcbw3sc1j25r5dilhyinjr58k3487zc1cn18smp2iydz"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;requires a running kwin session
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcmutils
           kconfigwidgets
           ki18n
           kwin
           kwindowsystem
           libepoxy
           libxcb
           qtbase
           wayland))
    (home-page "https://github.com/matinlotfali/KDE-Rounded-Corners")
    (synopsis "Round the corners for your windows in KDE Plasma")
    (description
     "This package provides a KWin effect for KDE Plasma that rounds the
corners of your windows and adds an outline around them without much affecting
the performance.")
    (license license:gpl3+)))

(define-public plasma-applet-chatai
  (package
    (name "plasma-applet-chatai")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/DenysMb/ChatAI-Plasmoid")
          (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wbqwngwczzdivrz5yp6jwwskskh6fhsz9gld51q0qwg98zfpg5q"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/plasma/plasmoids/ChatAI-Plasmoid"
           #:include-regexp ("metadata\\.json" "contents/")))))
    (propagated-inputs
     (list kdeclarative
           kcmutils
           kirigami
           libplasma
           knotifications
           qtbase
           qtdeclarative
           qtwebengine))
    (home-page "https://store.kde.org/p/2163340")
    (synopsis "Range of chatbots for KDE Plasma")
    (description
     "This package provides a widget for KDE Plasma that provides a range of
chatbots such as @uref{https://duck.ai/, Duck.ai}.")
    (license license:gpl2+)))

(define-public plasma-applet-kde-material-you-colors
  (package
    (inherit kde-material-you-colors)
    (name "plasma-applet-kde-material-you-colors")
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:configure-flags
      #~(list "-DINSTALL_PLASMOID=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "src"
                (with-directory-excursion "plasmoid/package/contents/ui"
                  (substitute* "FullRepresentation.qml"
                    ;; Check for the running process under the real command
                    ;; name.
                    (("(ps .* \\+ )execName" _ keep)
                     (string-append keep "\".\" + execName + \"-real\""))
                    (("findExecutable\\(execName\\)")
                     (string-append "findExecutable(execName, \""
                                    #$(this-package-input
                                       "kde-material-you-colors")
                                    "/bin\")"))
                    (("\\/usr") #$output)
                    (("(\"|\\$\\()find " _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/find")
                                    " "))
                    (("(-exec |')test" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/test")))
                    (("(-exec |\\$\\()grep" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/grep")))
                    (("(-exec )sh" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/sh")))
                    (("(\\| )wc" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/wc")))
                    (("(do )echo" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/echo")))
                    (("(\\$\\()basename" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs
                                                       "/bin/basename")))
                    (("(\\| )sed" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/sed")))
                    (("(\\| )sort" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/sort")))
                    (("(')ps" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/ps")))
                    (("(\\| )awk" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/awk")))
                    (("(\")sha1sum" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs "/bin/sha1sum")))
                    (("(\")cat" _ prefix)
                     (string-append prefix
                                    (search-input-file inputs
                                                       "/bin/cat")))))))))))
    (propagated-inputs
     (list kiconthemes
           kirigami
           ksvg
           libplasma
           plasma5support
           qt5compat
           qtbase
           qtdeclarative))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list bash-minimal
           coreutils-minimal
           findutils
           gawk
           grep
           kde-material-you-colors
           kservice
           plasma-workspace
           procps
           sed))
    (home-page "https://store.kde.org/p/2136963")))

(define-public plasma-applet-kurve
  (package
    (name "plasma-applet-kurve")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/luisbocanegra/kurve")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vghdlrrdc2fc8qrph9annmjcg2ibyfy2mp3xqvvn4f3y3vgscl3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:configure-flags
      #~(list "-DBUILD_PLUGIN=ON"
              "-DINSTALL_PLASMOID=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "package/contents/ui"
                (substitute* "Cava.qml"
                  (("(exec )cava" _ keep)
                   (string-append keep (which "cava"))))
                (substitute* "components/ProcessMonitorPrimary.qml"
                  (("\"sh\"")
                   (string-append "\"" (which "sh") "\""))))))
          (add-after 'install 'make-tools-executable
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o755))
                        (find-files (string-append #$output
                                                   "/share/plasma/plasmoids/"
                                                   "luisbocanegra."
                                                   "audio.visualizer"
                                                   "/contents/ui/tools"))))))))
    (propagated-inputs
     (list kcmutils
           ki18n
           kirigami
           libplasma
           plasma5support
           python-websockets
           plasma-workspace
           qtbase
           qtdeclarative
           qtwebsockets))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list bash-minimal cava python-minimal))
    (home-page "https://store.kde.org/p/2299506")
    (synopsis "Audio visualizer widget for KDE Plasma")
    (description
     "This package provides an audio visualizer widget, powered by @code{cava},
for KDE Plasma.")
    (license license:gpl3+)))

(define-public plasma-applet-panel-colorizer
  (package
    (name "plasma-applet-panel-colorizer")
    (version "6.8.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/luisbocanegra/plasma-panel-colorizer")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00fpmpgzifbkjjg0r7yksmgmhj0a72x3kkiird6k2yipxr1kvxkq"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:configure-flags
      #~(list "-DBUILD_PLUGIN=ON"
              "-DINSTALL_PLASMOID=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-tools-executable
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o755))
                        (find-files (string-append #$output
                                                   "/share/plasma/plasmoids/"
                                                   "luisbocanegra."
                                                   "panel.colorizer"
                                                   "/contents/ui/tools")))))
          (add-after 'install 'wrap-tools
            (lambda _
              (for-each (lambda (file)
                          (wrap-program file
                            `("PATH" ":" prefix
                              (,#$(file-append (this-package-input
                                                "coreutils-minimal")
                                               "/bin")
                               ,#$(file-append (gexp-input (this-package-input
                                                            "glib")
                                                           "bin")
                                               "/bin")))))
                        (find-files (string-append #$output
                                                   "/share/plasma/plasmoids/"
                                                   "luisbocanegra."
                                                   "panel.colorizer"
                                                   "/contents/ui/tools")
                                    "\\.sh$")))))))
    (native-inputs
     (list extra-cmake-modules))
    (propagated-inputs
     (list kcmutils
           ki18n
           kirigami
           ksvg
           libplasma
           plasma5support
           plasma-activities
           plasma-workspace
           python-dbus
           python-pygobject
           qtbase
           qt5compat
           qtdeclarative))
    (inputs
     (list bash-minimal
           coreutils-minimal
           `(,glib "bin")    ;for gdbus
           python-minimal-wrapper))
    (home-page "https://store.kde.org/p/2130967")
    (synopsis "Panel customizer for KDE Plasma")
    (description
     "This package provides Latte-Dock and WM status bar customization features
for KDE Plasma panels.")
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public plasma-applet-window-buttons
  (package
    (name "plasma-applet-window-buttons")
    (properties '((revision . "0")
                  ;; The latest tagged version does not support KDecoration3.
                  ;; So this commit is picked from the master branch.
                  (commit . "b114cf23da4411d19c1f1600a98bfab5369fd950")))
    (version (git-version "0.14.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/moodyhunter/applet-window-buttons6")
          (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "165prymhrfk6nqq357qcq8yzvb5lakwaf1zs53krl1q4jgvizamf"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-cmake
            (lambda _
              ;; kdecoration headers include C++20 spaceship operator.
              (substitute* "CMakeLists.txt"
                (("CMAKE_CXX_STANDARD 14") "CMAKE_CXX_STANDARD 20"))
              (substitute* "libappletdecoration/CMakeLists.txt"
                (("(kwin_xml )\\/usr" _ keep)
                 (string-append keep #$(this-package-input "kwin")))))))))
    (native-inputs
     (list extra-cmake-modules))
    (propagated-inputs
     (list kcmutils
           kconfigwidgets
           kcoreaddons
           kdeclarative
           kdecoration
           ki18n
           kirigami
           kitemmodels
           kpackage
           kservice
           ksvg
           kwin
           libplasma
           plasma-workspace
           qtbase
           qtdeclarative))
    (home-page "https://github.com/moodyhunter/applet-window-buttons6")
    (synopsis "Window buttons widget for KDE Plasma")
    (description
     "This package provides a widget for KDE Plasma that shows window
buttons of the active window.")
    (license license:gpl2+)))

(define-public plasma-applet-window-title
  (package
    (name "plasma-applet-window-title")
    (version "0.21")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/dhruv8sh/plasma6-window-title-applet")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vrm5v09gpnd0l1ik7hj2x1980z9x0qss70rw17vivvv5iidaqnq"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "/share/plasma/plasmoids/org.kde.windowtitle"
           #:include-regexp ("/contents/" "metadata.json")))))
    (propagated-inputs
     (list kirigami
           libplasma
           plasma-activities
           plasma-workspace
           qtdeclarative))
    (home-page "https://github.com/dhruv8sh/plasma6-window-title-applet")
    (synopsis "Window title widget for KDE Plasma")
    (description
     "This package provides a widget for KDE Plasma that shows the application
title and the icon of the active window.")
    (license license:gpl2+)))

(define-public plasma-wallpaper-active-blur
  (package
    (name "plasma-wallpaper-active-blur")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/bouteillerAlan/blurredwallpaper")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "066jbvg51zwsmhk67wnlbirgb6ynjkbr5fi230dm5pmhkb8c8isg"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("a2n.blur" "/share/plasma/wallpapers/a2n.blur"))))
    (propagated-inputs
     (list kcmutils
           kirigami
           kdeclarative
           knewstuff
           kwindowsystem
           libplasma
           plasma-activities
           plasma5support
           plasma-workspace
           qt5compat
           qtbase
           qtdeclarative))
    (home-page "https://github.com/bouteillerAlan/blurredwallpaper")
    (synopsis "Blurred wallpaper plugin for KDE Plasma")
    (description
     "This package provides a wallpaper plugin for KDE Plasma that blurs the
wallpaper when a window is active.")
    (license license:gpl2+)))

(define-public plasma-wallpaper-smart-video-wallpaper-reborn
  (package
    (name "plasma-wallpaper-smart-video-wallpaper-reborn")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url (string-append "https://github.com/luisbocanegra/"
                              "plasma-smart-video-wallpaper-reborn"))
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fscmvn3xg491fmm5iyy2q07lvwqggmh7wnn23h5v5dnnzhbm6h1"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "package/contents/ui"
                (substitute* "tools/gdbus_get_signal.sh"
                  (("gdbus") (which "gdbus"))))))
          (add-after 'install 'make-tools-executable
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o755))
                        (find-files (string-append #$output
                                                   "/share/plasma/plasmoids/"
                                                   "luisbocanegra.smart.video."
                                                   "wallpaper.reborn"
                                                   "/contents/ui/tools"))))))))
    (propagated-inputs
     (list kdeclarative
           ki18n
           kirigami
           libplasma
           plasma-workspace
           plasma5support
           qtbase
           qtdeclarative
           qtmultimedia))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list bash-minimal
           `(,glib "bin"))) ;for gdbus
    (home-page "https://store.kde.org/p/2139746")
    (synopsis "Video wallpaper plugin for KDE Plasma")
    (description
     "This package provides a wallpaper plugin for KDE Plasma to play videos on
your desktop or lock screen.")
    (license license:gpl2+)))

(define-public plasma-wallpaper-wallhaven-reborn
  (package
    (name "plasma-wallpaper-wallhaven-reborn")
    ;; No tags are available. This commit is taken from the master branch.
    (properties '((commit . "7f0220af0e6d4600be19e62bf5757b426787dfc8")
                  (revision . "0")))
    ;; Version is taken from file:///package/metadata.json.
    (version (git-version "1.1.1"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url (string-append "https://github.com/Blacksuan19/"
                              "plasma-wallpaper-wallhaven-reborn"))
          (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ccqps83x4k241z17zk6rhc034bh6dirqjcw219sq40gm35ix56s"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("package"
           "share/plasma/wallpapers/com.plasma.wallpaper.wallhaven"))))
    (propagated-inputs
     (list kdeclarative
           kirigami
           knotifications
           libplasma
           qtbase
           qtdeclarative))
    (home-page "https://store.kde.org/p/2193590")
    (synopsis "Wallhaven wallpaper plugin for KDE Plasma")
    (description
     "This package provides a wallpaper plugin for KDE Plasma that loads
images from @uref{https://wallhaven.cc/, Wallhaven}.")
    (license license:gpl2+)))
