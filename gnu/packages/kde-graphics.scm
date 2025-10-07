;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages kde-graphics)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public gwenview
  (package
    (name "gwenview")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/gwenview-" version ".tar.xz"))
       (sha256
        (base32 "0z6ngbb1pkgdy7z6nla788wh7wcz3ny64041yv7k0falzpwfx4jd"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("placetreemodeltest"
                                         "historymodeltest"
                                         "contextmanagertest"
                                         "urlutilstest")
                                       "|")
                          ")")))
    (native-inputs
     (list extra-cmake-modules
           kdoctools
           pkg-config))
    (inputs
     (list baloo
           cfitsio
           exiv2
           plasma-activities
           kcolorpicker
           kcrash
           kguiaddons
           ki18n
           kiconthemes
           kimageannotator
           kio
           kitemmodels
           knotifications
           kparts
           lcms
           libjpeg-turbo
           libkdcraw
           libpng
           libtiff
           libxkbcommon
           phonon
           purpose
           qtimageformats
           qtmultimedia
           qtsvg
           qtwayland
           wayland
           wayland-protocols
           zlib))
    (home-page "https://userbase.kde.org/Gwenview")
    (synopsis "Image viewer for KDE")
    (description
     "Gwenview is an image viewer for KDE.  It also provides image editing and
annotating features.")
    (license license:gpl2+)))

(define-public kcolorchooser
  (package
    (name "kcolorchooser")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kcolorchooser-" version ".tar.xz"))
       (sha256
        (base32
         "0899fdfni0pqr0nkbl54v503lhd5ngiw1ybgwmq1fagf6z004wwh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons
           ki18n
           kxmlgui
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/de/kcolorchooser/")
    (synopsis "Color selector utility")
    (description "KColorChooser is a utility to select a color.")
    (license license:expat)))

(define-public kdegraphics-mobipocket
  (package
    (name "kdegraphics-mobipocket")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "086922qpjr7bi6mxfi51r6dkk1nxslkxr154l0chrzfiyhk62i8r"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kio qtbase qt5compat))
    (home-page "https://apps.kde.org/en/kdegraphics_mobipocket")
    (synopsis "KDE thumbnailer for Mobipocket files")
    (description "This package provides a KDE plugin that shows thumbnails of
Mobipocket e-books in Dolphin and other KDE apps.")
    (license license:gpl2+)))

(define-public kdegraphics-thumbnailers
  (package
    (name "kdegraphics-thumbnailers")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "17b4ns2l03dl6lsnwanjva542qlldlc6ncf6nji0m8whq56qw10c"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags
      #~'("-DQT_MAJOR_VERSION=6")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "ps/gscreator.cpp"
                (("\"gs\",")
                 (string-append "\""
                                (search-input-file inputs "bin/gs") "\","))
                (("\"dvips\",")
                 (string-append "\""
                                (search-input-file inputs "bin/dvips") "\","))))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list ghostscript
                  karchive
                  kdegraphics-mobipocket
                  kio
                  libkdcraw
                  libkexiv2
                  qtbase
                  texlive-dvips-bin))
    (home-page "https://apps.kde.org/kdegraphics_thumbnailers")
    (synopsis "KDE thumbnailer for media files")
    (description "These plugins allow KDE software to display thumbnails for
PostScript, PDF, RAW, Mobipocket, and Blender files.")
    (license license:gpl2)))

(define-public kdiagram
  (package
    (name "kdiagram")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdiagram/" version
                           "/kdiagram-" version ".tar.xz"))
       (sha256
        (base32 "0vcw339v6nl1haznp58spimanfhw143cindbym1q3ccxrp1b0na6"))
       (patches (search-patches
                 "kdiagram-Fix-missing-link-libraries.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/graphics/kdiagram")
    (synopsis "Libraries for creating business diagrams")
    (description "This package provides libraries for integrating business
diagrams in Qt-based applications.

@code{KCharts} provides an implementation of the ODF Chart specification.  It
supports stock charts, box charts, and whisker charts.  @code{KGantt} provides
a module for implementing ODF Gantt charts, which are bar charts that
illustrate project schedules.")
    (license license:gpl2+)))

(define-public kgraphviewer
  (package
    (name "kgraphviewer")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/kgraphviewer/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0s9b3q7wvrbz52d500mcaflkrfjwhbzh5bsf5gxzgxjdzdiywaw7"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase
           boost
           graphviz
           ki18n
           kiconthemes
           kparts
           qt5compat
           qtsvg
           qtwayland))
    (native-inputs
     (list pkg-config extra-cmake-modules kdoctools))
    (arguments (list #:tests? #f))
    (home-page "https://apps.kde.org/kgraphviewer/")
    (synopsis "Graphviz dot graph viewer for KDE")
    (description "KGraphViewer is a Graphviz DOT graph file viewer, aimed to
replace the other outdated Graphviz tools.")
    (license license:gpl2+)))

(define-public kolourpaint
  (package
    (name "kolourpaint")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolourpaint-" version ".tar.xz"))
       (sha256
        (base32 "05xxcbfhn28896jk4dv8gq892fwjq501gbab7q5lry2nvazsqgnh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           kguiaddons
           ki18n
           kio
           kjobwidgets
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libksane
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "http://kolourpaint.org/")
    (synopsis "Paint program for KDE")
    (description "KolourPaint is a paint program for KDE.  It is useful for
painting, image manipulating and icon editing.")
    (license (list license:lgpl2.0+ license:bsd-2))))

(define-public krita
  (package
    (name "krita")
    (version "5.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://kde/stable/krita/" version "/krita-" version
             ".tar.gz"))
       (sha256
        (base32 "0camc7wk3285sxaam6idaxifx4b6hxv3vhgihh3g2awyr4q9946b"))
       (patches (search-patches "krita-bump-sip-abi-version-to-12.8.patch"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DCMAKE_CXX_FLAGS=-fPIC")
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-bin
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (python-path (getenv "GUIX_PYTHONPATH")))
                        (wrap-program (string-append out "/bin/krita")
                          `("GUIX_PYTHONPATH" ":" prefix (,python-path)))))))))
    (native-inputs
     (list curl
           eigen
           extra-cmake-modules
           gettext-minimal
           kitemmodels
           pkg-config
           qwt
           vc))
    (inputs
     (list bash-minimal
           boost
           exiv2
           fontconfig
           fftw-cmake
           ;; fftw
           ;; We use fftw-cmake since fftwm doesn't provide the required
           ;; CMake files when build with gnu.
           ;; See: https://bugzilla.redhat.com/show_bug.cgi?id=1729652#c5
           freetype
           fribidi
           giflib
           gsl
           harfbuzz
           imath
           immer
           karchive-5
           kcompletion-5
           kconfig-5
           kcoreaddons-5
           kcrash-5
           kguiaddons-5
           ki18n-5
           kiconthemes-5
           kio-5
           kitemviews-5
           kseexpr
           kwidgetsaddons-5
           kwindowsystem-5
           kxmlgui-5
           lager
           lcms
           libheif
           libjpeg-turbo
           libjxl
           libkdcraw-qt5
           libmypaint
           libpng
           ;; libraqm
           ;; We use the provided 3rd_party_vendor library instead of
           ;; libraqm 0.10.1 with patches until libraqm is patched.
           ;; See: https://github.com/HOST-Oman/libraqm/issues/191
           libraw
           libtiff
           libunibreak
           libwebp
           libx11
           libxcb
           libxi
           mlt
           opencolorio
           openexr
           openjpeg
           perl
           poppler-qt5
           python-pyqt
           python-pyqt5-sip
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtsvg-5
           qtwayland-5
           qtx11extras
           quazip-5
           sdl2
           xsimd
           zlib
           zug))
    (home-page "https://krita.org")
    (synopsis "Digital painting application")
    (description
     "Krita is a professional painting tool designed for concept artists,
illustrators, matte and texture artists, and the VFX industry.  Notable
features include brush stabilizers, brush engines and wrap-around mode.")
    (license license:gpl2+)))

(define-public kseexpr
  (package
    (name "kseexpr")
    (version "4.0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://invent.kde.org/graphics/kseexpr")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "888q3kkv2wq426w000iq14wy3a45rrnn0bmsdks6caz4vq04ccay"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DBUILD_TESTS=ON"))) ; disabled by default
    (native-inputs
     (list bison doxygen extra-cmake-modules flex googletest))
    (inputs
     (list ki18n libpng qtbase-5))
    (home-page "https://invent.kde.org/graphics/kseexpr")
    (synopsis "Embeddable expression evaluation engine")
    (description "This package contains the fork of Disney Animation's SeExpr
expression library, that is used in Krita.")
    (license license:gpl3+)))

(define-public libksane
  (package
    (name "libksane")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                             "/src/libksane-" version ".tar.xz"))
       (sha256
        (base32 "0ynczlnxdmf0agdv0h7xzvvr8zz3i5f84pcl91f9fbjspgij8h5d"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list ki18n
           ksanecore
           ktextwidgets
           kwallet
           kwidgetsaddons))
    (home-page "https://invent.kde.org/graphics/libksane")
    (synopsis "Library providing QWidget with logic to interface scanners")
    (description
     "Libksane is a Qt-based interface for SANE library to control flat
scanners.")
    (license license:lgpl3+)))

(define-public libkexiv2
  (package
    (name "libkexiv2")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "12kbfjbkac8hgkhpwg92hr5yq3lkp5pr7v4rqcsczzdx8b8kj2wm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list exiv2 qtbase))
    (home-page "https://invent.kde.org/graphics/libkexiv2")
    (synopsis "Manipulate the metadata of images")
    (description "Libkexiv2 wraps the Exiv2 library, allowing to manipulate
picture metadata as EXIF/IPTC and XMP.")
    (license license:gpl2+)))

(define-public okular
  (package
    (name "okular")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "027dcckqrm2k4ayqrl2dwyg718mi07sq5ghz6yf8gn6gqfhl5gk7"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:test-exclude
      (string-append "("
                     (string-join '("annotationtoolbartest"
                                    "mainshelltest"
                                    "parttest"
                                    "chmgeneratortest")
                                  "|")
                     ")")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          ;; use installed data to check.
          (add-after 'install 'check
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (invoke "ctest"
                        "--output-on-failure"
                        "--rerun-failed"
                        "-E"
                        test-exclude))))
          (add-before 'check 'check-setup
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share")))
                (setenv "QT_QPA_PLATFORM" "offscreen")
                (setenv "HOME" ".")
                (setenv "XDG_DATA_DIRS"
                        (string-append
                         share ":" (getenv "XDG_DATA_DIRS")))
                (invoke "update-desktop-database" "-v" share)))))))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config
           ;; for test
           desktop-file-utils
           python-wrapper))
    (inputs
     (list ebook-tools
           breeze-icons
           discount
           djvulibre
           plasma-activities
           chmlib
           kdegraphics-mobipocket
           karchive
           kbookmarks
           kcompletion
           kconfig
           libjpeg-turbo
           libtiff
           kirigami
           purpose
           freetype
           ki18n
           kiconthemes
           kio
           kparts
           kpty
           ktextwidgets
           qtspeech
           kwallet
           kwindowsystem
           libkexiv2
           libspectre
           libzip
           libxkbcommon
           phonon
           poppler-qt6
           qca
           qtdeclarative
           qtsvg
           qtwayland
           threadweaver
           kcrash))
    (home-page "https://apps.kde.org/okular/")
    (synopsis "Document viewer")
    (description
     "Okular is a document viewer developed for KDE.  It can display files in
a variety of formats, including PDF, PostScript, DejaVu, and EPub.")
    (license license:gpl2+)))
