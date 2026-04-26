;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2023 Parnikkapore <poomklao@yahoo.com>
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

(define-module (gnu packages stenography)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xorg))

(define-public python-plover-stroke
  (package
    (name "python-plover-stroke")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/benoit-pierre/plover_stroke")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104ab1lr2xr8nbq98n7h3jvskfzzg8m41rhb1ik4b7w474rlxgh3"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/benoit-pierre/plover_stroke")
    (synopsis "Stroke handling helper library for Plover")
    (description
     "This package provides a helper class for working with steno strokes.")
    (license license:gpl2+)))

(define-public plover
  (package
    (name "plover")
    (version "5.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstenoproject/plover")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dx5afcfqskwzkf6zfpii704f2b21z6al3d0g2h4xwza82cmknnl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list
                        "-p" "pytest-qt"
                        "-p" "xvfb"
                        "test"
                        ;; FIXME: Ignore failing test.
                        "--ignore" "test/gui_qt/test_dictionaries_widget.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: adjust after packaging pyside-6-tools.
          (add-after 'unpack 'set-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "plover_build_utils/setup.py"
                (("\"pyside6-(rcc|uic)\"" all prog)
                 (simple-format #f "~s, \"-g\", \"python\""
                   (search-input-file
                    inputs (string-append "/lib/qt6/libexec/" prog)))))))
          ;; FIXME: test_i18n_files_up_to_date would not pass
          ;; if the phase is run after build.
          (add-after 'build 'update-i18n-catalog-for-tests
            (lambda _
              (invoke "python" "setup.py" "extract_messages" "update_catalog")))
          ;; Ensure that icons are found at runtime.
          (add-after 'wrap 'wrap-executable
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/plover")
                `("QT_PLUGIN_PATH" prefix
                  (,(string-append #$(this-package-input "qtwayland")
                                   "/lib/qt6/plugins/")
                   ,(string-append #$(this-package-input "qtsvg")
                                    "/lib/qt6/plugins/")
                   ,(string-append #$(this-package-input "python-pyside-6")
                                   "/lib/qt6/plugins/")))
                `("LD_LIBRARY_PATH" prefix
                  (,(string-append #$(this-package-input "dbus") "/lib"))))))
          (add-after 'install 'install-desktop
            (lambda _
              (install-file "plover/assets/plover.png"
                            (string-append
                             #$output "/share/icons/hicolor/128x128/apps"))
              (let ((desktop
                     (string-append
                      #$output "/share/applications/plover.desktop")))
                (install-file "linux/plover.desktop" (dirname desktop))
                (substitute* desktop
                  (("Exec=plover")
                   (string-append "Exec=" #$output "/bin/plover"))))))
          (add-after 'install 'install-udev-rules
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (rules (string-append out "/lib/udev/rules.d")))
                (mkdir-p rules)
                (call-with-output-file
                    (string-append rules "/99-plover-uinput.rules")
                  (lambda (port)
                    (display
                     (string-append
                      "KERNEL==\"uinput\", MODE=\"0660\", "
                      "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"\n")
                     port)))))))))
    (native-inputs
     (list python-babel
           python-mock
           python-pytest
           python-pytest-qt
           python-pytest-xvfb
           python-setuptools))
    (inputs
     (list bash-minimal
           dbus
           qtbase
           qtsvg
           qtwayland
           xhost))
    (propagated-inputs
     (list python-appdirs
           python-dbus-1.2
           python-evdev
           python-hidapi
           python-packaging
           python-pkginfo
           python-plover-stroke
           python-psutil
           python-pygments
           python-pyserial
           python-pyside-6
           python-readme-renderer
           python-requests-cache
           python-requests-futures
           python-rtf-tokenize
           python-wcwidth
           python-xkbcommon
           python-xlib))
    (home-page "https://www.openstenoproject.org/plover/")
    (synopsis "Stenography engine")
    (description
     "Plover (rhymes with @emph{lover}) is a desktop application that
allows anyone to use stenography to write on their computer, up to
speeds of 200WPM and beyond.")
    (license license:gpl2+)))

