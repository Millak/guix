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

(define-module (gnu packages kde-education)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline))

(define-public analitza
  (package
    (name "analitza")
    (version "25.08.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/analitza")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q9jfkvs1xyxl7fmw0jzh7fh7rd278825i5w61cikqpwnjmp5jzf"))))
    (native-inputs (list extra-cmake-modules qttools))
    (inputs (list eigen qtbase qtdeclarative qtsvg))
    (build-system qt-build-system)
    (home-page "https://invent.kde.org/education/analitza")
    (synopsis "Library to add mathematical features to a program")
    (description "Analitza is a library to work with mathematical objects.
It adds mathematical features to your program, such as symbolic computations
and some numerical methods; for instance the library can parse mathematical
expressions and let you evaluate and draw them.")
    (license license:gpl2+)))

(define-public kalgebra
  (package
    (name "kalgebra")
    (version "24.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/education/kalgebra")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g4rrq3csp0w6xhc5cbbilz7xhhq9zdngc8bc9d16p02xz61qd4i"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-qt-process-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bin (string-append #$output "/bin/kalgebra"))
                    (qt-process-path
                     (search-input-file
                      inputs "/lib/qt6/libexec/QtWebEngineProcess")))
                (wrap-program bin
                  `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list analitza
           kconfigwidgets
           kcoreaddons
           kdoctools
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libplasma
           ncurses
           qtbase
           qtdeclarative
           qtsvg
           qtwebengine
           qtwebchannel
           readline))
    (home-page "https://invent.kde.org/education/kalgebra")
    (synopsis "Calculator and plotting tool")
    (description "KAlgebra is a calculator that lets you plot different types
of 2D and 3D functions and to calculate easy (and not so easy) calculations,
such as addition, trigonometric functions or derivatives.")
    (license license:gpl2+)))
