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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xorg))

(define-public python-plover-stroke
  (package
    (name "python-plover-stroke")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plover_stroke" version))
              (sha256
               (base32
                "0lyifam9xqpx2jzqcbah84sv909n4g2frm7pd5gvcrpf98zv40yy"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest))
    (home-page "https://github.com/benoit-pierre/plover_stroke")
    (synopsis "Stroke handling helper library for Plover")
    (description "This package provides a helper class for working with steno strokes.")
    (license license:gpl2+)))

(define-public plover
  (package
    (name "plover")
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstenoproject/plover")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vipma8jqn0ypjy5zm185ld10kxnz7c8ywr4cz5qdwa5p80yr07n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "-m" "pytest"
                        "-p" "pytest-qt"
                        "-p" "xvfb"
                        "test"
                        ;; FIXME: Ignore failing test.
                        "--ignore" "test/gui_qt/test_dictionaries_widget.py"))))
          ;; Ensure that icons are found at runtime.
          (add-after 'wrap 'wrap-executable
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/plover")
                `("QT_PLUGIN_PATH" prefix
                  (,(search-input-directory inputs "/lib/qt5/plugins/")))
                `("LD_LIBRARY_PATH" prefix
                  (,(string-append #$(this-package-input "dbus") "/lib"))))))
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
           python-evdev
           python-mock
           python-pytest
           python-pytest-qt
           python-pytest-xvfb
           python-wheel))
    (inputs
     (list bash-minimal
           dbus
           python-appdirs
           python-dbus
           python-hidapi
           python-plover-stroke
           python-pyqt
           python-pyserial
           python-rtf-tokenize
           python-wcwidth
           python-xlib
           qtsvg-5
           xhost))
    (home-page "https://www.openstenoproject.org/plover/")
    (synopsis "Stenography engine")
    (description
     "Plover (rhymes with @emph{lover}) is a desktop application that
allows anyone to use stenography to write on their computer, up to
speeds of 200WPM and beyond.")
    (license license:gpl2+)))

