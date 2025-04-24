;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg))

(define-public python-nbclassic
  (package
    (name "python-nbclassic")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbclassic" version))
       (sha256
        (base32 "1qrhzazq10dz64y9mawr3ns595fsdhrj1wvbb42xhmcl66r1xq8a"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-argon2-cffi
                             python-ipykernel
                             python-ipython-genutils
                             python-jinja2
                             python-jupyter-client
                             python-jupyter-core
                             python-jupyter-server
                             python-nbconvert
                             python-nbformat
                             python-nest-asyncio
                             python-notebook-shim
                             python-prometheus-client
                             python-pyzmq
                             python-send2trash
                             python-terminado
                             python-tornado-6
                             python-traitlets))
    (native-inputs (list python-coverage
                         python-nbval
                         python-jupyter-packaging
                         python-pytest
                         python-pytest-cov
                         python-pytest-jupyter
                         python-pytest-tornasync
                         python-requests
                         python-requests-unixsocket2
                         python-testpath))
    (home-page "https://github.com/jupyter/nbclassic")
    (synopsis "Jupyter Notebook as a Jupyter Server extension")
    (description "NbClassic provides a backwards compatible Jupyter Notebook
interface that you can install side-by-side with the latest versions: That
way, you can fearlessly upgrade without worrying about your classic extensions
and customizations breaking.  Because NbClassic provides the classic interface
on top of the new Jupyter Server backend, it can coexist with other frontends
like JupyterLab and Notebook 7 in the same installation.  NbClassic preserves
the custom classic notebook experience under a new set of URL endpoints, under
the namespace @code{/nbclassic/}.")
    (license license:bsd-3)))

(define-public python-notebook-shim
  (package
    (name "python-notebook-shim")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "notebook_shim" version))
       (sha256
        (base32 "1jrqqrm5xjwsx13plyyh7wybb1g71yrzaqa3l9y3162xnshwzcml"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-jupyter-server))
    (native-inputs
     (list python-hatchling
           python-pytest
           python-pytest-console-scripts
           python-pytest-jupyter
           python-pytest-tornasync))
    (home-page "https://pypi.org/project/notebook-shim/")
    (synopsis "Shim layer for notebook traits and config")
    (description
     "This project provides a way for JupyterLab and other frontends to switch
to Jupyter Server for their Python Web application backend.")
    (license license:bsd-3)))

(define-public python-jupyter-lsp
  (package
    (name "python-jupyter-lsp")
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter-lsp" version))
       (sha256
        (base32 "00ahai7wp0m98glpqsrd1bymcllzkb8irvskzl4zhinlbah4fcbr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; No R language server is present.
      '(list "-k" "not test_r_package_detection")
      #:phases
      '(modify-phases %standard-phases
         ;; Some tests require a writable HOME
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-jupyter-server))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://pypi.org/project/jupyter-lsp/")
    (synopsis "Multi-Language Server WebSocket proxy for Jupyter Notebook/Lab server")
    (description
     "This package provides a multi-language server WebSocket proxy for
Jupyter Notebook/Lab server.  It provides coding assistance for
JupyterLab (code navigation, hover suggestions, linters, autocompletion, and
rename) using the Language Server Protocol.")
    (license license:bsd-3)))

(define-public python-jupyter-protocol
  (package
    (name "python-jupyter-protocol")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_protocol" version))
              (sha256
               (base32
                "075vbaak6hlk9606lw61ldv72p6694k938jd1kvkm6spd0pczpmn"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-dateutil
           python-ipython-genutils
           python-jupyter-core
           python-pyzmq
           python-traitlets))
    (native-inputs
     (list python-ipykernel python-ipython python-mock python-pytest))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter protocol implementation")
    (description
     "This Python library is an experimental implementation of the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
protocol} to be used by both clients and kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_protocol")))))

(define-public python-jupyter-kernel-mgmt
  (package
    (name "python-jupyter-kernel-mgmt")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_kernel_mgmt" version))
              (sha256
               (base32
                "0977ixfi1pzjgy84hl0zycg4wpllmid98fhzcpy0lxd322w4sl7x"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; There are 8 test failures, most of them in 'test_client_loop.py'
      ;; (see: https://github.com/takluyver/jupyter_kernel_mgmt/issues/48).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv" "jupyter_kernel_mgmt")))))))
    (propagated-inputs
     (list python-dateutil
           python-entrypoints
           python-jupyter-core
           python-jupyter-protocol
           python-pyzmq
           python-tornado-6
           python-traitlets))
    (native-inputs
     (list python-async-generator
           python-ipykernel
           python-ipython
           python-pytest
           python-pytest-asyncio))
    (home-page "https://jupyter.org")
    (synopsis "Discover, launch, and communicate with Jupyter kernels")
    (description
     "This package is an experimental refactoring of the machinery for
launching and using Jupyter kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_kernel_mgmt")))))

(define-public python-jupyter-kernel-test
  (package
    (name "python-jupyter-kernel-test")
    (version "0.3")
    (home-page "https://github.com/jupyter/jupyter_kernel_test")
    (source (origin
              ;; PyPI has a ".whl" file but not a proper source release.
              ;; Thus, fetch code from Git.
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00iy74i4i8is6axb9vlsm0b9wxkvyyxnbl8r0i4gaj3xd788jm83"))))
    (build-system python-build-system)
    (arguments
     ;; The repo doesn't contain a "setup.py" file so install files manually.
     '(#:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (version (python-version (assoc-ref inputs "python")))
                             (pydir (string-append out "/lib/python"
                                                   version "/site-packages/"
                                                   "jupyter_kernel_test")))
                        (for-each (lambda (file)
                                    (install-file file pydir))
                                  (find-files "jupyter_kernel_test"
                                              "\\.py$"))
                        #t))))))
    (propagated-inputs
     (list python-jupyter-kernel-mgmt python-jupyter-protocol
           python-jsonschema))
    (synopsis "Test Jupyter kernels")
    (description
     "@code{jupyter_kernel_test} is a tool for testing Jupyter kernels.  It
tests kernels for successful code execution and conformance with the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
Messaging Protocol}.")
    (license license:bsd-3)))

(define-public python-pytest-jupyter
  (package
    (name "python-pytest-jupyter")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_jupyter" version))
       (sha256
        (base32 "114y9py29j6p2iymhc3vj55x65gg1ncbhwal5mads0g2z7p59pq0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This requires python-jupyter-server, which itself requires this
      ;; package.
      '(list "--ignore=tests/test_jupyter_server.py")
      #:phases
      '(modify-phases %standard-phases
         ;; The jupyter_server tests requires python-jupyter-server, which
         ;; itself requires this package.
         (add-after 'unpack 'disable-tests
           (lambda _
             (substitute* "tests/conftest.py"
               (("\"pytest_jupyter.jupyter_server\",") ""))))
         ;; Some tests require a writable HOME
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp")))
         (add-after 'unpack 'ignore-deprecation-warnings
           (lambda _
             (substitute* "pyproject.toml"
               (("  \"module:datetime.*" m)
                (string-append m "\n\"ignore:zmq.eventloop.ioloop is deprecated:DeprecationWarning\","))))))))
    (propagated-inputs (list python-jupyter-core python-nbformat))
    (native-inputs (list python-hatchling
                         python-ipykernel
                         python-pytest
                         python-pytest-timeout
                         python-tornado-6))
    (home-page "https://jupyter.org")
    (synopsis "Pytest plugin for testing Jupyter libraries and extensions.")
    (description
     "This package provides a pytest plugin for testing Jupyter libraries and
extensions.")
    (license license:bsd-4)))

(define-public xeus
  (package
    (name "xeus")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyter-xeus/xeus")
                    (commit version)))
              (sha256
               (base32
                "1k1h416qkw3yra6ayfa61nv0v4ify2wvp5x27slgbcw6c88w7fb1"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_STATIC_LIBS=OFF"
                           "-DDISABLE_ARCH_NATIVE=ON" ;no '-march=native'
                           "-DBUILD_TESTING=ON")))
    (native-inputs
     (list pkg-config
           ;; The following inputs are used by the test suite.
           googletest
           python-pytest
           python-wrapper
           python-jupyter-kernel-test
           python-jupyter-client))
    (inputs
     (list xtl
           nlohmann-json
           cppzmq
           zeromq
           openssl
           `(,util-linux "lib")))       ;libuuid
    (home-page "https://quantstack.net/xeus")
    (synopsis "C++ implementation of the Jupyter Kernel protocol")
    (description
     "@code{xeus} is a library meant to facilitate the implementation of
kernels for Jupyter.  It takes the burden of implementing the Jupyter Kernel
protocol so developers can focus on implementing the interpreter part of the
kernel.

Several Jupyter kernels are built upon @code{xeus}, such as @code{xeus-cling},
a kernel for the C++ programming language, and @code{xeus-python}, an
alternative Python kernel for Jupyter.")
    (license license:bsd-3)))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_pygments" version))
       (sha256
        (base32
         "0ij14mmnc39nmf84i0av6j9glazjic7wzv1qyhr0j5966s3s1kfg"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are no tests
    (propagated-inputs
     (list python-pygments))
    (home-page "https://jupyter.org")
    (synopsis "Pygments theme using JupyterLab CSS variables")
    (description
     "This package contains a syntax coloring theme for pygments making use of
the JupyterLab CSS variables.")
    (license license:bsd-3)))

(define-public python-jupyterlab-server
  (package
    (name "python-jupyterlab-server")
    (version "2.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_server" version))
       (sha256
        (base32 "07b3m34akrf79xpaim9cymhsac0ry5ry7if998lcfxmn173mlyq9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: These tests appear to fail due to the lack of
      ;; locales.
      '(list "-k" "not locale and not language")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'ignore-deprecation-warnings
           (lambda _
             (substitute* "pyproject.toml"
               (("  \"module:datetime.*" m)
                (string-append
                 m
                 "\n\"ignore:zmq.eventloop.ioloop is deprecated:DeprecationWarning\","
                 "\n\"ignore:There is no current event loop:DeprecationWarning\","
                 "\n\"ignore:Spec is deprecated. Use SchemaPath from jsonschema-path package.:DeprecationWarning\"," )))))
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-babel
           python-importlib-metadata    ;TODO: remove after Python >= 3.10
           python-jinja2
           python-json5
           python-jsonschema
           python-jupyter-server
           python-packaging
           python-requests))
    (native-inputs
     (list python-hatchling
           python-ipykernel
           python-openapi-core
           python-openapi-spec-validator
           python-pytest
           python-pytest-console-scripts
           python-pytest-cov
           python-pytest-jupyter
           python-pytest-timeout
           python-requests-mock
           python-ruamel.yaml
           python-strict-rfc3339
           python-werkzeug
           python-wheel))
    (home-page "https://jupyter.org")
    (synopsis "Server components for JupyterLab applications")
    (description "JupyterLab Server sits between JupyterLab and Jupyter
Server, and provides a set of REST API handlers and utilities that are used by
JupyterLab.  It is a separate project in order to accommodate creating
JupyterLab-like applications from a more limited scope.")
    (license license:bsd-3)))

(define-public python-jupyter-events
  (package
    (name "python-jupyter-events")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_events" version))
       (sha256
        (base32 "08jyhj16drl3hg594gr677bc5q991lpd4khlhb3jx26csclq42v7"))))
    (build-system pyproject-build-system)
    (arguments
     ;; This passes the whole command line to shutil.which, instead of just
     ;; the executable.
     (list #:test-flags '(list "--ignore=tests/test_cli.py")))
    (propagated-inputs (list python-jsonschema
                             python-json-logger
                             python-pyyaml
                             python-referencing
                             python-rich
                             python-rfc3339-validator
                             python-rfc3986-validator
                             python-traitlets))
    (native-inputs (list python-click
                         python-hatchling
                         python-pytest
                         python-pytest-asyncio
                         python-pytest-console-scripts))
    (home-page "https://pypi.org/project/jupyter-events/")
    (synopsis "Jupyter Event System library")
    (description "Jupyter Events enables Jupyter Python
Applications (e.g. Jupyter Server, JupyterLab Server, JupyterHub, etc.) to
emit events—structured data describing things happening inside the
application.  Other software (e.g. client applications like JupyterLab) can
listen and respond to these events.")
    (license license:bsd-3)))

(define-public python-jupyter-packaging
  (package
    (name "python-jupyter-packaging")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_packaging" version))
       (sha256
        (base32
         "1b7ssc627vgrdl21c09w9sxk5fc1ps3g7f70laxag4yw1bb5ax5j"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Disable isolation so that the package environment can be
                ;; setup without connectivity.
                (setenv "SOURCE_DATE_EPOCH" "315532800")
                (substitute* "tests/test_build_api.py"
                  (("\"-m\", \"build\"" all)
                   (string-append all ", \"--no-isolation\"")))
                (invoke "python" "-m" "pytest" "-vv")))))))
    (propagated-inputs
     (list python-deprecation python-packaging python-setuptools
           python-tomlkit python-wheel))
    (native-inputs
     (list python-pypa-build python-coverage python-pytest
           python-pytest-cov python-pytest-mock))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter packaging utilities")
    (description "This package provides tools to help build and install
Jupyter Python packages that require a pre-build step that may include
JavaScript build steps.")
    (license license:bsd-3)))

(define-public python-hatch-jupyter-builder
  (package
    (name "python-hatch-jupyter-builder")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hatch_jupyter_builder" version))
       (sha256
        (base32 "1baqk80c5ddhksh73l48mb59vvaaa2ldrs65k6vldii4s6c829vr"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Don't attempt to build anything, because we don't have network access
     ;; anyway.
     (list #:test-flags '(list "-k" "not test_hatch_build")))
    (propagated-inputs (list python-hatchling))
    (native-inputs (list python-pytest
                         python-pytest-cov
                         python-pytest-mock
                         python-tomli
                         python-twine))
    (home-page "https://github.com/jupyterlab/hatch-jupyter-builder")
    (synopsis "Hatch plugin to help build Jupyter packages")
    (description
     "This package provides a hatch plugin to help build Jupyter packages.")
    (license license:bsd-3)))

(define-public python-jupyter-server
  (package
    (name "python-jupyter-server")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server" version))
       (sha256
        (base32
         "0xz69anflhib514lgpdrs0ppmbwp13zbg4vwzls3820jlp7594b5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Integration tests require a server.
      '(list "-m" "not integration_test"
             ;; This test fails just like the shutil test in
             ;; python-jupyter-events fails.  Odd, that.
             "-k" "not test_server_extension_list")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'ignore-deprecation-warnings
            (lambda _
              (substitute* "pyproject.toml"
                (("  \"ignore:datetime.*" m)
                 (string-append m "\n\"ignore:zmq.eventloop.ioloop is deprecated:DeprecationWarning\","
                                "\n\"ignore:There is no current event loop:DeprecationWarning\","
                                "\n\"ignore:unclosed event loop:ResourceWarning\","
                                ;; From tornado
                                "\n\"ignore:unclosed <socket.socket:ResourceWarning\",")))))
          (add-before 'check 'pre-check
            (lambda _
              (let ((home (string-append (getcwd) "/guix-home")))
                (setenv "HOME" home))
              ;; Add jupyter-server executable to PATH.
              (setenv "PATH"
                      (string-append #$output "/bin:" (getenv "PATH"))))))))
    (propagated-inputs
     (list python-anyio
           python-argon2-cffi
           python-jinja2
           python-jupyter-client
           python-jupyter-core
           python-jupyter-events
           python-jupyter-server-terminals
           python-nbconvert
           python-nbformat
           python-overrides
           python-packaging
           python-prometheus-client
           python-pyzmq
           python-send2trash
           python-terminado
           python-tornado-6
           python-traitlets
           python-websocket-client))
    (native-inputs
     (list python-flaky
           python-hatchling
           python-hatch-jupyter-builder
           python-ipykernel
           python-pytest
           python-pytest-console-scripts
           python-pytest-jupyter
           python-pytest-timeout
           python-requests))
    (home-page "https://jupyter.org")
    (synopsis "Core services, APIs, and REST endpoints for Jupyter web applications")
    (description
     "This package provides the backend—i.e. core services, APIs, and REST
endpoints—to Jupyter web applications.")
    (license license:expat)))

(define-public python-jupyter-server-terminals
  (package
    (name "python-jupyter-server-terminals")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server_terminals" version))
       (sha256
        (base32 "0sajiadhwncrr0inzzkrs7l1xc6jmw9b5zfw1v79l3i2cx8jkq2s"))))
    (build-system pyproject-build-system)
    ;; The tests require python-jupyter-server, but python-jupyter-server
    ;; needs this package.
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-terminado))
    (native-inputs
     (list python-hatchling))
    (home-page "https://pypi.org/project/jupyter-server-terminals/")
    (synopsis "Jupyter Server extension providing terminals")
    (description
     "This package provides a Jupyter Server extension providing terminals.")
    (license license:bsd-3)))

(define-public python-jupyterlab-widgets
  (package
    (name "python-jupyterlab-widgets")
    (version "3.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_widgets" version))
       (sha256
        (base32
         "1h04kln8hp56svdjjk2hbsb0z1mby71cv4gss3wy89v7jw2arwh4"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-jupyter-packaging))
    (home-page "https://github.com/jupyter-widgets/ipywidgets")
    (synopsis "Interactive widgets for Jupyter Notebooks")
    (description "ipywidgets, also known as jupyter-widgets or simply widgets,
are interactive HTML widgets for Jupyter notebooks and the IPython kernel.")
    (license license:bsd-3)))

(define-public python-jupyterlite-core
  (package
    (name "python-jupyterlite-core")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlite_core" version))
       (sha256
        (base32 "18ysrqlsh7a31sknfnng419r7wpx9nfj59lxxd6zl1kcj6wazh34"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-doit python-importlib-metadata
                             python-jupyter-core))
    (native-inputs (list python-ansi2html
                         python-hatchling
                         python-pytest-console-scripts
                         python-pytest-cov
                         python-pytest-xdist
                         python-tornado-6))
    (home-page "https://github.com/jupyterlite/jupyterlite")
    (synopsis "Core functionality for building JupyterLite websites")
    (description "The jupyterlite-core package provides the core functionality
for building JupyterLite websites, the jupyter-lite CLI, and extension points
for authoring custom addons.")
    (license license:bsd-3)))

(define-public python-jupyterlite-sphinx
  (package
    (name "python-jupyterlite-sphinx")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlite_sphinx" version))
       (sha256
        (base32 "05h7zrvsx0xzxlddsqfz1fxdgld66yhn6nxfp4jz20wbx6csrr4v"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false))  ;there are none
    (propagated-inputs (list python-docutils python-jupyter-server
                             python-jupyterlab-server python-jupyterlite-core
                             python-sphinx))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/jupyterlite/jupyterlite")
    (synopsis "Sphinx extension for deploying JupyterLite")
    (description "This package provides a Sphinx extension for deploying
@code{JupyterLite}.")
    (license license:bsd-3)))

(define-public python-jupyter-server-mathjax
  (package
    (name "python-jupyter-server-mathjax")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server_mathjax" version))
       (sha256
        (base32 "0hrrl969r7ir6q683hlr7a4lid9x2s35hax2hviiyv38q1nnn7mv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "--pyargs" "jupyter_server_mathjax")))
    (propagated-inputs (list python-jupyter-server))
    (native-inputs
     (list python-jupyter-packaging
           python-pytest python-pytest-jupyter
           python-setuptools python-wheel))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter Server extension for serving Mathjax")
    (description "This package provides a Jupyter Server extension for serving
Mathjax, the JavaScript display engine for mathematics.")
    (license license:bsd-3)))

(define-public python-comm
  (package
    (name "python-comm")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)   ; no tests data in PyPi package
       (uri (git-reference
             (url "https://github.com/ipython/comm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18xsbpd8dgcfbc51xl59nlwaq7jnyzvgzjfj6psscv71894x4lg7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatchling python-pytest python-setuptools-scm))
    (propagated-inputs
     (list python-traitlets))
    (home-page "https://github.com/ipython/comm")
    (synopsis "Python Comm implementation for the Jupyter kernel protocol")
    (description
     "This package provides a way to register a Kernel Comm implementation, as
per the Jupyter kernel protocol. It also provides a base Comm implementation
and a default CommManager that can be used.")
    (license license:bsd-3)))

(define-public python-nbclient
  (package
    (name "python-nbclient")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbclient" version))
       (sha256
        (base32
         "02dvb9ffpd237apyj4fw97a9371kv99lzny49624j7jkp9yingsb"))))
    (build-system pyproject-build-system)
    ;; Tests require tools from nbconvert, which would introduces a cycle.
    (arguments '(#:tests? #false))
    (propagated-inputs
     (list python-jupyter-client python-jupyter-core python-nbformat
           python-traitlets))
    (native-inputs
     (list python-hatchling
           python-flaky
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-testpath
           python-xmltodict))
    (home-page "https://jupyter.org")
    (synopsis "Client library for executing notebooks")
    (description
     "This package provides a client library for executing notebooks.
It was formerly known as nbconvert's @code{ExecutePreprocessor.}")
    (license license:bsd-3)))

(define-public python-nbdime
  (package
    (name "python-nbdime")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbdime" version))
       (sha256
        (base32 "12v41lricbg713lzlfcx0cilfm9spndaanhp39q4ydvix4h76xk7"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-ipython-genutils
            ;; TODO: Remove when a release newer than 3.1.1 is made.
            (lambda _
              (substitute* "nbdime/config.py"
                (("from ipython_genutils import py3compat")
                 "")
                (("py3compat\\.getcwd")
                 "os.getcwd")))))))
    (propagated-inputs
     (list python-colorama
           python-gitpython
           python-jinja2
           python-jupyter-server
           python-jupyter-server-mathjax
           python-nbformat
           python-pygments
           python-requests
           python-tornado-6))
    (native-inputs
     (list python-jupyter-server
           python-mock
           python-notebook
           python-pytest
           python-pytest-tornado
           python-tabulate))
    (home-page "https://nbdime.readthedocs.io")
    (synopsis "Diff tools for Jupyter Notebooks")
    (description "@code{nbdime} provides tools for diffing and merging of
Jupyter Notebooks.  It includes the following commands:
@table @command
@item nbdiff compare notebooks in a terminal-friendly way
@item nbmerge three-way merge of notebooks with automatic conflict resolution
@item nbdiff-web rich rendered diff of notebooks
@item nbmerge-web web-based three-way merge tool for notebooks
nbshow present a single notebook in a terminal-friendly way
@end table")
    (license license:bsd-3)))

(define-public python-nbstripout
  (package
    (name "python-nbstripout")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nbstripout" version))
              (sha256
               (base32
                "1c8b4fz807qlh028yi35gahwbas4pbwc1wjx3vz8v7kj9rmqpb7a"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests use git.
      '(list "--ignore=tests/test_git_integration.py"
             ;; These complain about missing files.
             "--ignore=tests/test_end_to_end.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-CRAMSHELL
            (lambda _
              (setenv "CRAMSHELL" (which "bash")))))))
    (propagated-inputs (list python-nbformat))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/kynan/nbstripout")
    (synopsis "Strips outputs from Jupyter and IPython notebooks")
    (description
     "This package opens a notebook, strips its output, and writes the outputless
version to the original file.")
    (license license:expat)))

(define-public repo2docker
  (package
    (name "repo2docker")
    (version "2024.03.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyterhub/repo2docker/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bcnl91j6p3315lk2mmn02jq6mjsn68m9rcw5rkln4c9fx1160rx"))))
    (outputs '("out" "doc"))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-shebangs 'fix-install-miniforge
            (lambda _
              (substitute* (find-files
                            #$output "^(install-miniforge|install-nix|\
nix-shell-wrapper|repo2docker-entrypoint)")
                (("^#!(.*)/bin/bash")
                 "#!/bin/bash"))
                (substitute* (find-files #$output "^freeze\\.py$")
                  (("^#!(.*)/bin/python3")
                   "#!/bin/python3\n"))))
          (add-after 'install 'make-doc
            (lambda _
              (let ((doc (string-append #$output:out "/share/doc/"
                                        #$(package-name this-package))))
                (setenv "PYTHONPATH"
                        (string-append (getcwd) ":"
                                       (getenv "GUIX_PYTHONPATH")))
                ;; Don't treat warnings as errors.
                (substitute* "docs/Makefile"
                  (("(SPHINXOPTS[[:blank:]]+= )-W" _ group)
                   group))
                (with-directory-excursion "docs"
                  (invoke  "make" "html")
                  (copy-recursively "build/html"
                                    (string-append doc "/html")))))))))
    (inputs
     (list python-traitlets
           python-toml
           python-semver
           python-ruamel.yaml
           python-requests
           python-json-logger
           python-jinja2
           python-iso8601
           python-escapism
           python-docker
           python-chardet))
    (native-inputs
     (list python-entrypoints
           python-myst-parser
           python-pydata-sphinx-theme
           python-recommonmark
           python-setuptools
           python-sphinx
           python-sphinx-autobuild
           python-sphinx-copybutton
           python-sphinxcontrib-autoprogram
           python-sphinxext-opengraph
           python-sphinxext-rediraffe
           python-wheel))
    (home-page "https://repo2docker.readthedocs.io/en/latest/index.html#")
    (synopsis "Generate docker images from repositories")
    (description
     "repo2docker fetches a repository (from GitHub, GitLab, Zenodo, Figshare,
Dataverse installations, a Git repository or a local directory) and builds a
container image in which the code can be executed.  The image build process is
based on the configuration files found in the repository.  repo2docker can be
used to explore a repository locally by building and executing the constructed
image of the repository, or as a means of building images that are pushed to a
Docker registry.")
    (license license:bsd-3)))

(define-public python-bash-kernel
  (package
   (name "python-bash-kernel")
   (version "0.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "bash_kernel" version))
            (sha256
             (base32
              "0w0nbr3iqqsgpk83rgd0f5b02462bkyj2n0h6i9dwyc1vpnq9350"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'bash-references
          (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bash_kernel/kernel.py"
               (("\"bash\"")
                (string-append "\"" (assoc-ref inputs "bash") "/bin/bash\""))
               (("\\['bash', ")
                (string-append "['" (assoc-ref inputs "bash") "/bin/bash', ")))
             #t))
        (add-after 'install 'install-kernelspec
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "HOME" "/tmp")
              (invoke "python" "-m" "bash_kernel.install" "--prefix" out)
              #t))))))
   (inputs
     (list bash
           python-pexpect
           python-ipykernel
           python-jupyter-client))
   (home-page "https://github.com/takluyver/bash_kernel")
   (synopsis "Jupyter kernel for Bash")
   (description "A bash shell kernel for Jupyter.")
   (license license:expat)))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "004v22nyi5cnpxq4fiws89p7i5wcnzv45n3n70axdd6prh6rkapx"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'no-custom-css
            (lambda _
              (substitute* "sparqlkernel/install.py"
                (("notebook.DEFAULT_STATIC_FILES_PATH") "\"/does-not-matter\"")
                (("install_custom_css\\( destd, PKGNAME \\)") ""))))
          (add-after 'add-install-to-pythonpath 'install-kernelspec
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke
               (string-append #$output "/bin/jupyter-sparqlkernel")
               "install"
               (string-append "--InstallKernelSpec.prefix=" #$output)))))))
    (propagated-inputs
     (list python-ipykernel
           python-notebook
           python-pygments
           python-rdflib
           python-sparqlwrapper
           python-traitlets))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This module installs a Jupyter kernel for SPARQL.  It allows
sending queries to an SPARQL endpoint, fetching and presenting the results in
a notebook.")
    (license license:bsd-3)))

(define-public python-ipympl
  (package
    (name "python-ipympl")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipympl" version))
       (sha256
        (base32 "12qgiy08klqb5gipm23yzh09p5g2k8ihcq2bprprdya84acw2rf8"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-ipython
           python-ipython-genutils
           python-ipywidgets
           python-matplotlib
           python-numpy
           python-pillow
           python-traitlets))
    (native-inputs
     (list python-hatchling python-jupyter-packaging))
    (home-page "https://matplotlib.org/ipympl/")
    (synopsis "Matplotlib Jupyter Extension")
    (description "Leveraging the Jupyter interactive widgets framework, ipympl
enables the interactive features of matplotlib in the Jupyter notebook and in
JupyterLab.")
    (license license:bsd-3)))

(define-public python-ipydatawidgets
  (package
    (name "python-ipydatawidgets")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipydatawidgets" version))
       (sha256
        (base32 "1g65nzlsb1cipmvh9v27b22kkmzwvg8zbf32hmg1c25mb65vbr6h"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke
                "pytest" "-v"
                ;; Disable failing tests.
                "-k" (string-append
                      "not test_dataunion_constricts_widget_data"
                      " and not test_dataunion_widget_change_notified"
                      " and not test_datawidget_creation_blank_comm"
                      ;; TODO: type object 'Widget' has no attribute '_ipython_display_'
                      " and not test_notification"
                      " and not test_manual_notification"
                      " and not test_sync_segment"
                      " and not test_hold_sync"
                      " and not test_hold_sync_segment"))))))))
    (propagated-inputs
     (list python-ipython-genutils
           python-ipywidgets
           python-numpy
           python-traittypes))
    (native-inputs
     (list python-jupyter-packaging
           python-nbval
           python-pytest
           python-pytest-cov))
    (home-page "https://github.com/vidartf/ipydatawidgets")
    (synopsis "Widgets to help facilitate reuse of large datasets across widgets")
    (description
     "This package provides a set of widgets to help facilitate reuse of large
datasets across widgets.")
    (license license:bsd-3)))

(define-public python-papermill
  (package
    (name "python-papermill")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "papermill" version))
       (sha256
        (base32 "097ai2n7f72a7hya9qnds3f28cg70p8xdj2c3cwqymzx28cskqlz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Do not bother testing Azure, AWS, and Google Cloud features.
      '(list "--ignore=papermill/tests/test_abs.py"
             "--ignore=papermill/tests/test_adl.py"
             "--ignore=papermill/tests/test_gcs.py"
             "--ignore=papermill/tests/test_s3.py")))
    (propagated-inputs (list python-aiohttp
                             python-ansicolors
                             python-click
                             python-entrypoints
                             python-nbclient
                             python-nbformat
                             python-pyyaml
                             python-requests
                             python-tenacity
                             python-tqdm))
    (native-inputs (list python-attrs
                         python-black
                         python-boto3
                         python-botocore
                         python-bumpversion
                         python-check-manifest
                         python-codecov
                         python-coverage
                         python-ipython
                         python-ipywidgets
                         python-moto
                         python-notebook
                         python-pytest
                         python-pytest-cov
                         python-pytest-env
                         python-pytest-mock
                         python-recommonmark
                         python-requests
                         python-setuptools
                         python-tox
                         python-twine
                         python-wheel))
    (home-page "https://github.com/nteract/papermill")
    (synopsis "Parameterize and run Jupyter and nteract Notebooks")
    (description "Papermill is a tool for parameterizing, executing, and
analyzing Jupyter Notebooks.")
    (license license:bsd-3)))

(define-public python-qtconsole
  (package
    (name "python-qtconsole")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qtconsole" version))
       (sha256
        (base32 "1r8bznf8mlajh8rcrhikp694naq653nx4zw58f0yzlvmdiz1rbaw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; All tests fail with error: This plugin does not support
      ;; propagateSizeHints()
      #~(list "--ignore=qtconsole/tests/test_00_console_widget.py"
              ;; AssertionError: '<!DO[261 chars]size:12pt; font-weight:400;
              ;; font-style:normal;[1218 chars]tml>' != '<!DO[261
              ;; chars]size:9pt; font-weight:400; font-style:normal;"[1217
              ;; chars]tml>'Diff is 1756 characters long. Set self.maxDiff to
              ;; None to see it.
              "-k" "not test_other_output")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "PYTEST_QT_API" "pyqt5")
              (setenv "QT_LIB" "pyqt5")
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (system "Xvfb :1 -screen 0 640x480x24 &")
              (setenv "DISPLAY" ":1")
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-flaky
           python-pyqt
           python-pytest
           python-pytest-qt
           python-setuptools
           python-wheel
           xorg-server-for-tests))
    (propagated-inputs
     (list python-ipykernel
           python-jupyter-client
           python-jupyter-core
           python-packaging
           python-pygments
           python-qtpy
           python-traitlets))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter Qt console")
    (description
     "This package provides a Qt-based console for Jupyter with support for
rich media output.")
    (license license:bsd-3)))

(define-public python-voila
  (package
    (name "python-voila")
    (version "0.5.8")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/voila-dashboards/voila")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fxw7m03iqd4bj1075mx6bspl48nj1rddi4mbs9jkziadc5yv7in"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Many tests depend on Node JavaScript dependencies and a running HTTP
      ;; server; ignore them.
      #:test-flags
      '(list "--ignore" "tests/app"
             "--ignore" "tests/server"
             ;; No python3 jupyter kernel in the build environment.
             "-k" "not test_execute_output")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-css
            (lambda* (#:key inputs #:allow-other-keys)
              ;; FIXME: we skip the build of the JavaScript extension.  We
              ;; hadn't built it in previous versions, because we could easily
              ;; get away with it, but in this version we have to patch the
              ;; build system.
              (substitute* "pyproject.toml"
                (("\"voila/labextensions/jupyterlab-preview/static/style.js\",") "")
                (("\"share/jupyter/voila/themes/@jupyterlab/theme-dark-extension/index.css\"") ""))
              (copy-file (assoc-ref inputs "variables.css")
                         "share/jupyter/voila/templates/base/static/labvariables.css")
              (copy-file (assoc-ref inputs "materialcolors.css")
                         "share/jupyter/voila/templates/base/static/materialcolors.css")))
          ;; FIXME: This is likely wrong.  The official wheel has very
          ;; different contents, which must be the result of actually running
          ;; jlpm and building the JavaScript packages.
          (add-after 'install 'install-extensions
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((dir (string-append #$output
                                        "/share/jupyter/voila/labextensions/"
                                        "@voila-dashboards/widgets-manager7")))
                (mkdir-p dir)
                (invoke "tar" "xf"
                        (assoc-ref inputs
                                   (string-append "voila-dashboards-widgets-manager7-"
                                                  #$version ".tgz"))
                        "--strip-components=1" "-C" dir))
              (let ((dir (string-append #$output
                                        "/share/jupyter/voila/labextensions/"
                                        "@voila-dashboards/widgets-manager8")))
                (mkdir-p dir)
                (invoke "tar" "xf"
                        (assoc-ref inputs
                                   (string-append "voila-dashboards-widgets-manager8-"
                                                  #$version ".tgz"))
                        "--strip-components=1" "-C" dir))
              (let ((dir (string-append #$output
                                        "/share/jupyter/voila/labextensions/"
                                        "@voila-dashboards/jupyterlab-preview")))
                (mkdir-p dir)
                (invoke "tar" "xf"
                        (assoc-ref inputs
                                   "voila-dashboards-jupyterlab-preview-2.3.8.tgz")
                        "--strip-components=1" "-C" dir))))
          (add-before 'check 'set-HOME
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-jupyter-client
           python-jupyter-server
           python-jupyterlab-server
           python-nbclient
           python-nbconvert
           python-traitlets
           python-websockets))
    (native-inputs
     (list python-hatchling
           python-hatch-jupyter-builder
           python-ipywidgets
           python-matplotlib
           python-mock
           python-numpy
           python-pandas
           python-pytest
           python-pytest-tornasync
           python-tornado-6
           (origin
             (method url-fetch)
             (uri "https://unpkg.com/@jupyterlab/apputils@3.2.8/style/materialcolors.css")
             (sha256
              (base32
               "1kvb24r3hbhmjdiip09w9pgzv6xmjzndch279r3ppf7rkgdcgirs")))
           (origin
             (method url-fetch)
             (uri "https://unpkg.com/@jupyterlab/theme-light-extension@3.2.8/style/variables.css")
             (sha256
              (base32
               "1mq6pr8w1r4jb2jgav15kkmjbca0x3nfsdv7q40xai994gsw5ygi")))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/voila-dashboards/voila/releases/download/v"
                   version "/voila-dashboards-widgets-manager7-" version ".tgz"))
             (sha256
              (base32
               "14fzn89nd6fnixzsy8jhz1y40z82msj5n44242zsjfxhlq8203rm")))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/voila-dashboards/voila/releases/download/v"
                   version "/voila-dashboards-widgets-manager8-" version ".tgz"))
             (sha256
              (base32
               "09kdrzpiw5psdq0ybrmmav1bwbli9gb4bdg0507069sipkap1nh4")))
           ;; FIXME: we are not yet using this release tarball.
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/voila-dashboards/voila/releases/download/v"
                   version "/voila-dashboards-jupyterlab-preview-2.3.8.tgz"))
             (sha256
              (base32
               "1bj2v03183aksn0qcqvb6p6kh8p992pk0zyz1x4s2xpijyh0fxpm")))))
    (home-page "https://github.com/voila-dashboards/voila")
    (synopsis "Render live Jupyter notebooks with interactive widgets")
    (description
     "Voilà turns Jupyter notebooks into standalone web applications.  Unlike
the usual HTML-converted notebooks, each user connecting to the Voilà tornado
application gets a dedicated Jupyter kernel which can execute the callbacks to
changes in Jupyter interactive widgets.")
    (license license:bsd-3)))
