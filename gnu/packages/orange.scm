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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
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
  #:use-module (gnu packages time)
  #:use-module (gnu packages qt))

(define-public python-orange-canvas-core
  (package
    (name "python-orange-canvas-core")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange_canvas_core" version))
       (sha256
        (base32 "1c6xq4bl524p2n0p74iw35nwwfwnlhigvcqyqr9kbn4n4w7831ad"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 242 passed, 4 skipped, 2 deselected, 24 warnings
      #:test-flags
      #~(list "-k" (string-join
                    ;; There is no CanvasIcon.png icon file.
                    (list "not test_loader"
                          ;; This test fails with "AssertionError:
                          ;; <PyQt6.QtWidgets.QWidget object at
                          ;; 0x7ffed655df90> is not None".
                          "test_garbage_collect_widgets")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; Relax hard requirement of PIP.
                ((".*pip>=18.0.*") ""))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list python-pytest
           python-pytest-qt
           python-setuptools
           python-trubar))
    (propagated-inputs
     (list python-anyqt
           python-commonmark
           python-dictdiffer
           python-docutils
           python-numpy
           python-packaging
           python-qasync
           python-requests
           python-requests-cache
           python-truststore
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
    (version "4.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "orange_widget_base" version))
       (sha256
        (base32 "0njvk7ppmz2lfny373a8v574s8k3mxxqaaihwi3i78h42iq8mgcv"))))
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
                     "test_garbage_collect "
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
           python-trubar))
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
    (version "3.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/biolab/orange3")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x5gayvpmjzjfr1pm63qvk854mnwxd7rqdzkwwfzyl097afpv3sp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: Ran 5971 tests; 710  skipped, 1 expected failures
      #:test-backend #~'unittest
      #:test-flags #~(list "-v" "Orange.tests" "Orange.widgets.tests")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute*
                  (list "Orange/tests/test_url_reader.py"
                        "Orange/misc/tests/test_server_embedder.py"
                        "Orange/widgets/unsupervised/tests/test_owkmeans.py")
                (((string-join
                   ;; Network tests.
                   (list "test_base_url_with_query"
                         "test_basic_file"
                         "test_url_with_fragment"
                         "test_zipped"
                         "test_callbac"
                         "test_connection_error"
                         "test_different_models_caches"
                         "test_encode_data_instance"
                         "test_on_json_wrong_key_response"
                         "test_on_non_json_response"
                         "test_other_errors"
                         "test_persistent_caching"
                         "test_read_error"
                         "test_responses"
                         "test_responses_empty"
                         "test_retries"
                         "test_too_many_examples_for_one_batch"
                         ;; AssertionError: Lists differ <...>
                         ;; -  np.float64(0.10179851067061697),
                         ;; +  0.10908461268784905,
                         "test_optimization_fails")
                   "|") m)
                 (string-append "__off_" m)))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "QT_API" "PyQt6")
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (delete-file-recursively "Orange")))
          (add-after 'wrap 'wrap-executable
            ;; Ensure that icons are found at runtime.
            (lambda _
              (wrap-program (string-append #$output "/bin/orange-canvas")
                `("QT_PLUGIN_PATH" prefix
                  ,(list (string-append
                          (string-join
                           (list #$(this-package-input "qtbase")
                                 #$(this-package-input "qtsvg")
                                 #$(this-package-input "qtwayland"))
                           "/lib/qt6/plugins:")
                          "/lib/qt6/plugins")))))))))
    (native-inputs
     (list python-cython
           python-recommonmark
           python-setuptools
           python-trubar))
    (inputs
     (list bash-minimal ;for wrap
           python-baycomp
           python-bottleneck
           python-chardet
           python-httpx
           python-joblib
           python-keyring
           python-keyrings-alt
           python-louvain
           python-networkx
           python-numpy
           python-openpyxl
           python-opentsne
           python-packaging
           python-pandas
           python-pyyaml
           python-requests
           python-scikit-learn
           python-scipy
           python-serverfiles
           python-tzdata
           python-xgboost
           python-xlrd
           python-xlsxwriter
           ;; [optional] gui, pyqt, sql
           python-anyqt
           python-matplotlib
           python-orange-canvas-core
           python-orange-widget-base
           python-psycopg2
           python-pygments
           ;; python-pymssql     ;not packaged yet in Guix
           python-pyqt-6
           python-pyqtgraph
           python-pyqtwebengine-6
           python-qtconsole
           qtbase
           qtsvg
           qtwayland
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
    (build-system pyproject-build-system)
    (arguments (list #:test-backend #~'unittest))
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-requests))
    (home-page "https://github.com/biolab/serverfiles")
    (synopsis "Utility to access files on a HTTP server and store them locally")
    (description
     "This package provides an utility that accesses files on a HTTP server
and stores them locally for reuse.  It is primarily used by the Orange
framework.")
    (license license:gpl3+)))
