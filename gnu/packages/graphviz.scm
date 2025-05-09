;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2018, 2019, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Zheng Junjie <z572@z572.online>
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

(define-module (gnu packages graphviz)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public graphviz
  (package
    (name "graphviz")
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/api/v4/projects/4207231"
                                  "/packages/generic/graphviz-releases/"
                                  version "/graphviz-" version ".tar.xz"))
              (sha256
               (base32
                "1b6x3g03j7q77lzyvdp34hkzld5sg1l1ippc6sh1qxnmm59xs3ly"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: rtest/rtest.sh is a ksh script (!).  Add ksh as an input.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-guile-bindings
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((lib (string-append #$output "/lib"))
                          (extdir (string-append lib "/guile/"
                                                 #$(version-major+minor
                                                    (package-version
                                                     (this-package-input "guile")))
                                                 "/extensions")))
                     (mkdir-p extdir)
                     (rename-file (string-append
                                   lib "/graphviz/guile/libgv_guile.so")
                                  (string-append extdir
                                                 "/libgv_guile.so"))))))))
    (inputs
     (list libxrender
           libx11
           gts
           gd
           guile-3.0                    ;Guile bindings
           pango
           fontconfig
           freetype
           libltdl
           libxaw
           expat
           libjpeg-turbo
           libpng))
    (native-inputs
     (list bison
           pkg-config
           swig))
    (outputs '("out" "doc"))            ;5 MiB of html + pdfs
    (home-page "https://www.graphviz.org/")
    (synopsis "Graph visualization software")
    (description
     "Graphviz is a graph visualization tool suite.  Graph visualization is a
way of representing structural information as diagrams of abstract graphs and
networks.  It has important applications in networking, bioinformatics,
software engineering, database and web design, machine learning, and in visual
interfaces for other technical domains.")
    (properties
     '((release-monitoring-url . "https://graphviz.org/download/source/")))
    (license license:epl1.0)))

(define-public graphviz-minimal
  (package/inherit graphviz
    (name "graphviz-minimal")
    (inputs (modify-inputs (package-inputs graphviz)
              (delete "libxrender" "libx11" "pango" "libxaw")))
    (synopsis "Graph visualization software (without X11 support)")))

(define-public python-graphviz
  (package
    (name "python-graphviz")
    (version "0.20.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "graphviz" version ".zip"))
              (sha256
               (base32
                "0pcjnnhprs1hb4r9jr7r4qjxc7lzsjlka8d5gcp3kym9ws0vrmh9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'pathch-pytest-options
            (lambda _
              (substitute* "setup.cfg"
                ((".*doctest.*") "")
                (("--cov.*") ""))))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                  (apply invoke "python" "run-tests.py" test-flags)))))))
    (native-inputs
     (list unzip
           ;; For tests.
           graphviz
           python-mock
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-setuptools
           python-wheel))
    (home-page "https://github.com/xflr6/graphviz")
    (synopsis "Simple Python interface for Graphviz")
    (description
     "This package provides a simple Python interface for the Graphviz graph
visualization tool suite.")
    (license license:expat)))

(define-public python-pygraphviz
  (package
    (name "python-pygraphviz")
    (version "1.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pygraphviz/pygraphviz")
             (commit (string-append "pygraphviz-" version))))
       (file-name (string-append "pygraphviz-" version "-checkout"))
       (sha256
        (base32 "03q6k030nvrl30a86bgnjqjh1csh8zpw8dkgajkn33v3cx7jc9a7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "pygraphviz")))
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list graphviz))
    (home-page "https://pygraphviz.github.io")
    (synopsis "Python interface to Graphviz")
    (description
     "PyGraphviz is a Python interface to the Graphviz graph layout and
visualization package.  With PyGraphviz you can create, edit, read, write, and
draw graphs using Python to access the Graphviz graph data structure and
layout algorithms.")
    (license license:bsd-3)))

(define-public python-uqbar
  (package
    (name "python-uqbar")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josiah-wolf-oberholtzer/uqbar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1limp2m2smg0l3v6vn2fwhjcw1d8gakw5v0q7krb03q539qiql87"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "uqbar")))
    (native-inputs
     (list graphviz
           python-defusedxml
           python-pytest
           python-pytest-cov
           python-setuptools
           python-typing-extensions
           python-wheel))
    (propagated-inputs
     (list python-black
           python-sphinx
           python-unidecode))
    (home-page "https://github.com/josiah-wolf-oberholtzer/uqbar")
    (synopsis "Tools for building documentation with Sphinx, Graphviz and LaTeX")
    (description
     "This package contains tools for building documentation with Sphinx,
Graphviz and LaTeX.")
    (license license:expat)))

(define-public gts
  (package
    (name "gts")
    (version "0.7.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/gts/gts/" version
                                 "/gts-" version ".tar.gz"))
             (sha256
              (base32
               "07mqx09jxh8cv9753y2d2jsv7wp8vjmrd7zcfpbrddz3wc9kx705"))))
    (build-system gnu-build-system)
    (arguments
     `(,@(if (%current-target-system)
             `(#:configure-flags
               (list (string-append
                      "PKG_CONFIG="
                      (search-input-file
                       %build-inputs
                       (string-append "/bin/" ,(pkg-config-for-target))))))
             '())
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (chmod "test/boolean/test.sh" #o777)
             #t))
         ,@(if (%current-target-system)
               `((add-after 'unpack 'update-config
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (for-each (lambda (file)
                                 (install-file
                                  (search-input-file
                                   (or native-inputs inputs)
                                   (string-append "/bin/" file)) "."))
                               '("config.guess" "config.sub"))))
                 (add-after 'unpack 'fix-predicates_init
                   (lambda _
                     (substitute* "src/Makefile.in"
                       (("\\$[(]COMPILE[)] \\$[(]srcdir[)]/predicates_init\\.c\
 -o \\$[(]srcdir[)]/predicates_init")
                        "gcc $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $(srcdir)/predicates_init.c \
-o $(srcdir)/predicates_init")))))
               '()))

       ;; Some data files used by the test suite are missing.
       ;; See <http://sourceforge.net/p/gts/bugs/41/>.
       #:tests? #f))
    (native-inputs
     (append (if  (%current-target-system)
                  (list config)
                  '())
             (list pkg-config)))
    (propagated-inputs
     ;; The gts.pc file has glib-2.0 as required.
     (list glib))
    (home-page "https://gts.sourceforge.net/")

    ;; Note: Despite the name, this is not official GNU software.
    (synopsis "Triangulated Surface Library")
    (description
     "Library intended to provide a set of useful functions to deal with
3D surfaces meshed with interconnected triangles.")
    (license license:lgpl2.0+)))

(define-public xdot
  (package
    (name "xdot")
    (version "1.4")
    (source
     (origin
       ;; PyPI tarball is missing some test files.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jrfonseca/xdot.py/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l34ahvcz90j3j7aspp9wqvxpq10mzgq7l0sanrj142ihdnbahvy"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'start-xserver
                     (lambda* (#:key inputs native-inputs #:allow-other-keys)
                       (let ((Xvfb (search-input-file (or native-inputs inputs)
                                                      "/bin/Xvfb")))
                         (system (format #f "~a :1 -screen 0 640x480x24 &"
                                         Xvfb))
                         (setenv "DISPLAY" ":1"))))
                   (add-before 'check 'set-test-environment
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((lib (search-input-file inputs
                                                     "lib/libvulkan.so.1")))
                         (setenv "XDG_RUNTIME_DIR"
                                 (getcwd))
                         (setenv "LIBGL_ALWAYS_SOFTWARE" "1")
                         (format (current-error-port) "~a~%"
                                 (assoc-ref inputs "gtk+"))
                         (setenv "LD_LIBRARY_PATH"
                                 (dirname lib)))))
                   ;; We wrap xdot, so that we don't propagate gtk+ and graphviz
                   (add-after 'install 'wrap
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out"))
                             (graphviz (assoc-ref inputs "graphviz"))
                             (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                             (python-path (getenv "GUIX_PYTHONPATH")))
                         (wrap-program (string-append out "/bin/xdot")
                           `("PATH" ":" prefix
                             (,(string-append graphviz "/bin")))
                           `("GI_TYPELIB_PATH" ":" prefix
                             (,gi-typelib-path))
                           `("GUIX_PYTHONPATH" ":" prefix
                             (,python-path))))))
                   (replace 'check
                     (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                       (when tests?
                         (add-installed-pythonpath inputs outputs)
                         (invoke "python" "test.py")))))))
    (native-inputs (list gobject-introspection xorg-server-for-tests
                         python-packaging))
    (inputs (list at-spi2-core
                  bash-minimal
                  (librsvg-for-system)
                  mesa
                  mesa-opencl
                  harfbuzz
                  graphviz
                  gtk+
                  python-numpy
                  python-pycairo
                  python-pygobject
                  vulkan-loader))
    (home-page "https://pypi.org/project/xdot/")
    (synopsis "Interactive viewer for graphviz dot files")
    (description
     "Xdot is an interactive viewer for graphs written in
@code{graphviz}’s dot language.  Internally, it uses the xdot output format as
an intermediate format, and @code{gtk} and @code{cairo} for rendering.  Xdot
can be used either as a standalone application, or as a Python library.")
    (license license:lgpl3+)))

(define-public python-pydot
  (package
    (name "python-pydot")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydot" version))
       (sha256
        (base32
         "0z80zwldf7ffkwrpm28hixsiqp3053j7g281xd6phmnbkfiq3014"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Taken from .travis.yaml
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "test"
                 (invoke "python" "pydot_unittest.py"))))))))
    (native-inputs
     ;; For tests.
     (list graphviz python-chardet))
    (propagated-inputs
     ;; XXX: Two test failures with 3.0+:
     ;; https://github.com/pydot/pydot/issues/277
     (list python-pyparsing-2.4.7))
    (home-page "https://github.com/pydot/pydot")
    (synopsis "Python interface to Graphviz's DOT language")
    (description
     "Pydot provides an interface to create, handle, modify and process
graphs in Graphviz's DOT language, written in pure Python.")
    (license license:expat)))

(define-public dot2tex
  (package
    (name "dot2tex")
    (version "2.11.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dot2tex" version))
              (sha256
               (base32
                "1kp77wiv7b5qib82i3y3sn9r49rym43aaqm5aw1bwnzfbbq2m6i9"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests"
                        ;; The test_semicolon test fails for unknown reason
                        ;; (see:
                        ;; https://github.com/kjellmf/dot2tex/issues/94).
                        "-k" "not test_semicolon")))))))
    (native-inputs
     (list python-pytest
           (texlive-local-tree
            (list texlive-pgf
                  texlive-preview
                  texlive-pstricks
                  texlive-xcolor
                  texlive-xkeyval))))
    (inputs (list graphviz))
    (propagated-inputs (list python-pyparsing))
    (home-page "https://github.com/kjellmf/dot2tex")
    (synopsis "Graphviz to LaTeX converter")
    (description
     "The purpose of @code{dot2tex} is to give graphs generated by Graphviz a
more LaTeX friendly look and feel.  This is accomplished by converting
@code{xdot} output from Graphviz to a series of PSTricks or PGF/TikZ commands.
This approach allows:

@itemize @bullet
@item Typesetting labels with LaTeX, allowing mathematical notation
@item Using native PSTricks and PGF/TikZ commands for drawing arrows
@item Using backend specific styles to customize the output
@end itemize")
    (license license:expat)))

(define-public gprof2dot
  (package
    (name "gprof2dot")
    (version "2021.02.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jrfonseca/gprof2dot")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jjhsjf5fdi1fkn7mvhnzkh6cynl8gcjrygd3cya5mmda3akhzic"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "tests/test.py")))))))
    (native-inputs
     (list graphviz))
    (home-page "https://github.com/jrfonseca/gprof2dot")
    (synopsis "Generate a dot graph from the output of several profilers")
    (description "This package provides a Python script to convert the output
from many profilers into a dot graph.

It can:

@itemize

@item prune nodes and edges below a certain threshold;
@item use an heuristic to propagate time inside mutually recursive functions;
@item use color efficiently to draw attention to hot-spots;
@item work on any platform where Python and Graphviz is available.

@end itemize")
    (license license:lgpl3+)))
