;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages orange)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt))

(define-public python-orange-canvas-core
  (package
    (name "python-orange-canvas-core")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange_canvas_core" version))
       (sha256
        (base32 "0bp6c5y4a4fzr1hg7aijlbfwp6bqacxxcqhwb2swc21aj846ns0n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;; AttributeError: 'NoneType' object has no attribute
                     ;; 'trigger'
                     "not test_context_menu_delete"
                     "test_copy_cut_paste"
                     ;; AttributeError: 'NoneType' object has no attribute
                     ;; 'isEnabled'
                     "test_item_context_menu"
                     ;; Tests fail with error: Failed: CALL ERROR: Exceptions
                     ;; caught in Qt event loop.
                     "test_create_new_window"
                     "test_new_window"
                     "test_dont_load_swp_on_new_window"
                     "test_toolbox"
                     "test_widgettoolgrid"
                     "test_editlinksnode"
                     "test_links_edit"
                     "test_links_edit_widget"
                     "test_flattened"
                     "test_tooltree_registry")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; Relax hard requirment of PIP.
                ((".*pip>=18.0.*") ""))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "QT_PLUGIN_PATH"
                      (string-append #$(this-package-input "qtbase") "/lib/qt6/plugins:"
                                     (getenv "QT_PLUGIN_PATH")))
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pytest
           python-pytest-qt
           python-setuptools
           python-trubar
           python-wheel))
    (inputs
     (list qtbase))
    (propagated-inputs
     (list python-anyqt
           python-cachecontrol
           python-commonmark
           python-dictdiffer
           python-docutils
           python-numpy
           python-qasync
           python-requests
           python-requests-cache
           python-typing-extensions))
    (home-page "https://github.com/biolab/orange-canvas-core")
    (synopsis "Core component of Orange Canvas")
    (description
     "Orange Canvas Core is a framework for building graphical user interfaces
for editing workflows.  It is a component used to build the Orange Canvas
data-mining application.")
    (license license:gpl3)))

(define-public python-orange-widget-base
  (package
    (name "python-orange-widget-base")
    (version "4.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange_widget_base" version))
       (sha256
        (base32 "1vms6bf96dnx5ban30xd0r0ipc318iq9q2wwgnazphf4hic8vxzi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    (list
                     ;;  KeyError: 'HTML (*.html)'
                     "not test_save_report_html"
                     ;; KeyError: 'PDF (*.pdf)'
                     "test_save_report_pdf"
                     ;; AssertionError: False is not true
                     "test_menu")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-trubar
           python-wheel))
    (propagated-inputs
     (list python-anyqt
           python-matplotlib
           python-orange-canvas-core
           python-pyqtgraph
           python-typing-extensions))
    (home-page "https://github.com/biolab/orange-widget-base")
    (synopsis "Base Widget for Orange Canvas")
    (description
     "Orange Widget Base provides a base widget component for a interactive
GUI based workflow.  It is primarily used in the Orange framework.")
    (license license:gpl3+)))

(define-public orange
  (package
    (name "orange")
    (version "3.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Orange3" version))
       (sha256
        (base32 "0pxjwisc209cdgpqlqazc2vlmr0iqz8ry862w7jx95zic54d9p5l"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Delete test that opens an outgoing connection.
             (delete-file "Orange/tests/test_url_reader.py")
             ;; This is a binary data file and it breaks runpath validation.
             (delete-file "Orange/tests/datasets/binary-blob.tab")
             ;; Skip the test which uses that binary file.
             (substitute* "Orange/tests/test_txt_reader.py"
               (("test_read_nonutf8_encoding") "_test_read_nonutf8_encoding"))
             ;; We use a correct version of PyQtWebEngine, but the build scripts
             ;; consider it incorrect anyways. Remove the constraint entirely to
             ;; work around this bug.
             (substitute* "requirements-pyqt.txt" (("PyQtWebEngine>=5.12") ""))))
         (add-before 'check 'pre-check
           ;; Tests need a writable home.
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "QT_QPA_PLATFORM" "offscreen")))
         (add-after 'install 'wrap-executable
           ;; Ensure that icons are found at runtime.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/orange-canvas")
                 `("QT_PLUGIN_PATH" prefix
                   ,(list (string-append (assoc-ref inputs "qtsvg")
                                         "/lib/qt5/plugins/"))))))))))
    (native-inputs
     (list python-cython))
    (inputs
     (list bash-minimal
           python-anyqt
           python-baycomp
           python-bottleneck
           python-chardet
           python-httpx
           python-joblib
           python-keyring
           python-keyrings-alt
           python-louvain
           python-matplotlib
           python-networkx
           python-numpy
           python-openpyxl
           python-opentsne
           python-orange-canvas-core
           python-orange-widget-base
           python-pandas
           python-pygments
           python-pyqt
           python-pyqtgraph
           python-pyqtwebengine
           python-pyyaml
           python-qtconsole
           python-requests
           python-scikit-learn
           python-scipy
           python-serverfiles
           python-xlrd
           python-xlsxwriter
           qtbase-5
           qtsvg-5
           xdg-utils))
    (home-page "https://orangedatamining.com/")
    (synopsis "Component-based data mining framework")
    (description
     "Orange is a component-based, graphical framework for machine learning,
data analysis, data mining and data visualization.")
    (license license:gpl3+)))

(define-public python-serverfiles
  (package
    (name "python-serverfiles")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "serverfiles" version))
       (sha256
        (base32 "1qgbzgnaxj4wsp2rdas53qxsh0j7xpibq25w6lviwyaqwwrgq42y"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/biolab/serverfiles")
    (synopsis "Utility to access files on a HTTP server and store them locally")
    (description
     "This package provides an utility that accesses files on a HTTP server
and stores them locally for reuse.  It is primarily used by the Orange
framework.")
    (license license:gpl3+)))
