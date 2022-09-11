;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016-2019, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages sphinx)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages tex))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "1rp28jryxwy24y8vpacclqihbizyi6b1s6id86pibvm46ybcmy3v"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Requires Internet access.
               (delete-file "tests/test_build_linkcheck.py")
               (substitute* "tests/test_build_latex.py"
                 (("@pytest.mark.sphinx\\('latex', testroot='images'\\)")
                  "@pytest.mark.skip()"))
               (setenv "HOME" "/tmp")   ;for test_cython
               (invoke "make" "test")))))))
    (propagated-inputs
     (list python-babel
           python-docutils
           python-jinja2
           python-imagesize
           python-importlib-metadata
           python-packaging
           python-pygments
           python-requests
           python-snowballstemmer
           python-sphinx-alabaster-theme
           python-sphinxcontrib-applehelp
           python-sphinxcontrib-devhelp
           python-sphinxcontrib-htmlhelp
           python-sphinxcontrib-jsmath
           python-sphinxcontrib-qthelp
           python-sphinxcontrib-serializinghtml

           ;; The Sphinx LaTeX library '\RequirePackage' or \\usepackage
           ;; these:
           texlive-amsfonts             ;amsmath, amssymb, amstext
           texlive-amsmath
           texlive-capt-of
           texlive-carlisle             ;remreset
           texlive-etoolbox
           texlive-generic-ltxcmds
           texlive-hyperref
           ;; TODO: Remove texlive-stringenc and texlive-zapfding after
           ;; propagating them in texlive-hyperref in next rebuild cycle.
           texlive-stringenc
           texlive-zapfding
           texlive-latex-base           ;alltt, atbegshi, makeidx, textcomp
           texlive-latex-cmap
           texlive-latex-fancyhdr
           texlive-latex-fancyvrb
           texlive-latex-float
           texlive-latex-fncychap
           texlive-latex-framed
           texlive-latex-geometry
           texlive-latex-graphics       ;graphicx, color
           texlive-latex-kvoptions
           texlive-latex-needspace
           texlive-latex-parskip
           texlive-latex-preview
           texlive-latex-tabulary
           texlive-latex-titlesec
           texlive-latex-tools          ;multicol, longtable
           texlive-latex-upquote
           texlive-latex-varwidth
           texlive-oberdiek             ;hypcap
           texlive-wrapfig
           texlive-xcolor))
    (native-inputs
     (list imagemagick                  ;for "convert"
           python-cython
           python-html5lib
           python-pytest))
    (home-page "https://www.sphinx-doc.org")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-apidoc
  (package
    (name "python-sphinxcontrib-apidoc")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-apidoc" version))
       (sha256
        (base32
         "1f9zfzggs8a596jw51fpfmr149n05mrlyy859iydazbvry9gb6vj"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;requires python-pytest<4.0
    (native-inputs
     (list python-pbr
           python-pre-commit
           python-pytest
           python-sphinx
           python-testrepository))
    (home-page "https://github.com/sphinx-contrib/apidoc")
    (synopsis "Sphinx extension for running @code{sphinx-apidoc}")
    (description "This package provides Sphinx extension for running
@code{sphinx-apidoc} on each build.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-applehelp
  (package
    (name "python-sphinxcontrib-applehelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-applehelp" version))
              (sha256
               (base32
                "0n5wrn4l7x6gxvi1g7c6y72hkxgc223axz1jykipaxhfr1g76wm0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-applehelp")
    (synopsis "Sphinx extension for creating Apple help books")
    (description
     "@code{sphinxcontrib-applehelp} is a Sphinx extension which outputs
Apple help books.")
    (license license:bsd-2)))

(define-public python-sphinx-click
  (package
    (name "python-sphinx-click")
    (version "4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-click" version))
       (sha256
        (base32
         "1nqy3b7wr64rbmdp7kpi723az53a89y6250h46i505g1rw0czam1"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "pytest" "-vv" "tests")))))))
    (native-inputs (list python-pbr python-pytest python-wheel))
    (propagated-inputs (list python-click python-docutils python-sphinx))
    (home-page "https://github.com/click-contrib/sphinx-click")
    (synopsis "Sphinx extension that documents click applications")
    (description "This package provide sphinx extension that automatically
documents click applications.")
    (license license:expat)))

(define-public python-sphinx-copybutton
  (package
    (name "python-sphinx-copybutton")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-copybutton" version))
       (sha256
        (base32
         "1xl7jwcldqvfya2gdp1nfxma7rv35alk998dfnx2fg6hmpd5kh50"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; XXX: Check requires network access.
    (propagated-inputs (list python-sphinx))
    (home-page "https://github.com/choldgraf/sphinx-copybutton")
    (synopsis "Sphinx extension to add \"copy\" buttons to code blocks")
    (description
     "This package provides a small sphinx extension to add \"copy\" buttons
to code blocks.")
    (license license:expat)))

(define-public python-sphinxcontrib-devhelp
  (package
    (name "python-sphinxcontrib-devhelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-devhelp" version))
              (sha256
               (base32
                "1r1qngsbjqbg4rj93kpj44qqy7n4x5khldkr0c3ffhlnggx1lzzz"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-devhelp")
    (synopsis "Sphinx extension for creating Devhelp documents")
    (description
     "@code{sphinxcontrib-devhelp} is a Sphinx extension which outputs
@url{Devhelp,https://wiki.gnome.org/Apps/Devhelp} documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-github-alt
  (package
    (name "python-sphinxcontrib-github-alt")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_github_alt" version))
       (sha256
        (base32
         "1x9af78vamjjcdrrhiah3wg613jv7gm8yh9vvqfrmf4vam6mimyg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-docutils python-sphinx))
    (home-page "https://github.com/jupyter/sphinxcontrib_github_alt")
    (synopsis "Link to GitHub pages from Sphinx docs")
    (description
     "This package lets you link to GitHub issues, pull requests, commits and
users from Sphinx docs.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-htmlhelp
  (package
    (name "python-sphinxcontrib-htmlhelp")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-htmlhelp" version))
              (sha256
               (base32
                "1ckd5xx4ngd6f4prxbc1bbvnafy1gg06j3bxyj5kk7v21lnvpy7m"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-htmlhelp")
    (synopsis "Sphinx extension for rendering HTML help files")
    (description
     "@code{sphinxcontrib-htmlhelp} is a Sphinx extension which renders
HTML help files.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-jsmath
  (package
    (name "python-sphinxcontrib-jsmath")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-jsmath" version))
              (sha256
               (base32
                "1f64w19j33sp151jimibraw6qrbhd5gxy8hs3797w9478m55x4m9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-jsmath")
    (synopsis "Sphinx extension to render math equations")
    (description
     "@code{sphinxcontrib-jsmath} is a Sphinx extension which renders display
math in HTML via JavaScript.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-newsfeed
  (package
    (name "python-sphinxcontrib-newsfeed")
    (version "0.1.4")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "sphinxcontrib-newsfeed" version))
             (sha256
              (base32
               "1d7gam3mn8v4in4p16yn3v10vps7nnaz6ilw99j4klij39dqd37p"))))
    (arguments '(#:tests? #f)) ; No tests.
    (build-system python-build-system)
    (propagated-inputs
     (list python-sphinx))
    (synopsis "News Feed extension for Sphinx")
    (description "Sphinxcontrib-newsfeed is an extension for adding a simple
Blog, News or Announcements section to a Sphinx website.")
    (home-page "https://bitbucket.org/prometheus/sphinxcontrib-newsfeed")
    (license license:bsd-2)))

(define-public python-sphinx-panels
  (package
    (name "python-sphinx-panels")
    (version "0.6.0")
    (source
      (origin
        ;; Tests not included in the pypi release.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/executablebooks/sphinx-panels")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ivqz6yv96a2jp59kylg1gbkrmzq6zwilppz3ij0zrkjn25zb97k"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs (list python-docutils python-sphinx))
    (native-inputs
     (list python-pytest
           python-pytest-regressions))
    (home-page "https://github.com/executablebooks/sphinx-panels")
    (synopsis "Sphinx extension for creating panels in a grid layout")
    (description
     "This package provides a sphinx extension for creating panels in a grid layout.")
    (license license:expat)))

(define-public python-sphinx-tabs
  (package
    (name "python-sphinx-tabs")
    (version "3.4.1")
    (home-page "https://github.com/executablebooks/sphinx-tabs")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinx-tabs" version))
              (sha256
               (base32
                "0cmqw5ck2jcxqyf5ibz543idspq0g0fdzxh3fpah1r0nhfg9z86j"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                ;TODO: requires sphinx-testing and rinohtype
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'loosen-docutils-requirement
                    (lambda _
                      (substitute* "setup.py"
                        (("docutils~=0\\.18\\.0")
                         "docutils>=0.17.0")))))))
    (propagated-inputs
     (list python-docutils python-pygments python-sphinx))
    (synopsis "Tabbed views for Sphinx")
    (description
     "Create tabbed content in Sphinx documentation when building HTML.")
    (license license:expat)))

(define-public python-sphinxcontrib-programoutput
  (package
    (name "python-sphinxcontrib-programoutput")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-programoutput" version))
              (sha256
               (base32
                "0zrb2ny6y7nk84qmw5mds84fc4pxgqf4sjy7bk95b0zfrawfj3ih"))))
    (build-system python-build-system)
    (propagated-inputs  (list python-sphinx))
    (synopsis "Sphinx extension to include program output")
    (description "A Sphinx extension to literally insert the output of arbitrary
commands into documents, helping you to keep your command examples up to date.")
    (home-page "https://github.com/NextThought/sphinxcontrib-programoutput")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-qthelp
  (package
    (name "python-sphinxcontrib-qthelp")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-qthelp" version))
              (sha256
               (base32
                "0wjsp96d262shzkx7pb7pra7mmf0j8c5rz56i6x0vdsqw1z7ccsc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-qthelp")
    (synopsis "Sphinx extension to output QtHelp documents")
    (description
     "@code{sphinxcontrib-qthelp} is a Sphinx extension which outputs QtHelp
documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-serializinghtml
  (package
    (name "python-sphinxcontrib-serializinghtml")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-serializinghtml" version))
              (sha256
               (base32
                "0lk91cl9bi4ynhz971zjs0bsr7jwxx8mx2f40psrx06zvzjnspxa"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-serializinghtml")
    (synopsis "Sphinx extension to serialize HTML files")
    (description
     "@code{sphinxcontrib-serializinghtml} is a Sphinx extension which outputs
\"serialized\" HTML files.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-svg2pdfconverter
  (package
    (name "python-sphinxcontrib-svg2pdfconverter")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-svg2pdfconverter" version))
              (sha256
               (base32
                "07c5nmkyx2y0gwfjq66fhy68c24mclvs2qqv1z9ilvvypii4blb0"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))         ;no tests
    (propagated-inputs
     (list python-sphinx))
    (home-page
     "https://github.com/missinglinkelectronics/sphinxcontrib-svg2pdfconverter")
    (synopsis "Sphinx SVG to PDF converter extension")
    (description "A Sphinx extension to convert SVG images to PDF in case the
builder does not support SVG images natively (e.g. LaTeX).")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-websupport
  (package
    (name "python-sphinxcontrib-websupport")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-websupport" version))
              (sha256
               (base32
                "0ck2jphvs82vjcbphhd1h7j1xfi9ynv5d8g5b947qnk8l0ih5psf"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests depend on Sphinx, which depends on this.
     `(#:tests? #f))
    (propagated-inputs
     (list python-sphinxcontrib-serializinghtml))
    (home-page "https://sphinx-doc.org/")
    (synopsis "Sphinx API for web applications")
    (description
     "This package provides a Python API to easily integrate
Sphinx documentation into your web application.  It provides tools to
integrate Sphinx documents in web templates and to handle searches.")
    (license license:bsd-3)))


(define-public python-sphinx-gallery
  (package
    (name "python-sphinx-gallery")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-gallery" version))
       (sha256
        (base32 "1r07sa34511fbnwi2s32q00qdyv5d23d05imyfgnh2ivhfq34gwm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'writable-files-for-tests
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "sphinx_gallery" "-k"
                       (string-append
                        ;; These tests require online data.
                        "not test_embed_code_links_get_data"
                        " and not test_run_sphinx"
                        ;; AssertionError.
                        " and not test_embed_links_and_styles"))))))))
    (native-inputs
     (list python-joblib
           python-matplotlib
           python-numpy
           python-pillow
           python-pytest
           python-pytest-cov
           python-sphinx))
    (home-page "https://sphinx-gallery.github.io/stable/index.html")
    (synopsis "Generate an examples gallery automatically")
    (description
     "@code{sphinx_gallery} is a Sphinx extension that builds an HTML version
from any set of Python scripts and puts it into an examples gallery.")
    (license license:bsd-3)))

(define-public python-sphinx-me
  (package
    (name "python-sphinx-me")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-me" version))
       (sha256
        (base32
         "06jzgp213zihnvpcy2y5jy3ykid3apc2ncp2pg6a2g05lhiziglq"))))
    (build-system python-build-system)
    (home-page "https://github.com/stephenmcd/sphinx-me")
    (synopsis "Create a Sphinx documentation shell")
    (description
      "Create a Sphinx documentation shell for your project and include the
README file as the documentation index.  It handles extracting the required
meta data such as the project name, author and version from your project for
use in your Sphinx docs.")
    (license license:bsd-2)))

(define-public python-sphinx-repoze-autointerface
  (package
    (name "python-sphinx-repoze-autointerface")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "repoze.sphinx.autointerface" version))
              (sha256
               (base32
                "08ycivzf7bh4a1zcyp31hbyqs1b2c9r26raa3vxjwwmbfqr3iw4f"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs
     (list python-sphinx python-zope-interface))
    (synopsis "Auto-generate Sphinx API docs from Zope interfaces")
    (description "This package defines an extension for the Sphinx documentation
system.  The extension allows generation of API documentation by
introspection of @code{zope.interface} instances in code.")
    (home-page "https://github.com/repoze/repoze.sphinx.autointerface")
    (license license:repoze)))

(define-public python-sphinx-prompt
  (package
    (name "python-sphinx-prompt")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)               ; no source release in PyPI
       (uri (git-reference
             (url "https://github.com/sbrunner/sphinx-prompt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x9wmgf04rzivbzp7jv1b7fkhkpi02lpk5w1qf4i7bcgih00ym8a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-pytest python-sphinx))
    (home-page "https://github.com/sbrunner/sphinx-prompt")
    (synopsis "Sphinx directive to add unselectable prompt")
    (description
     "This package provides a Sphinx directive to add unselectable prompt.")
    (license license:bsd-3)))

(define-public python-sphinx-alabaster-theme
  (package
    (name "python-sphinx-alabaster-theme")
    (version "0.7.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "alabaster" version))
              (sha256
               (base32
                "00nwwjj2d2ym4s2kk217x7jkx1hnczc3fvm8yxbqmsp6b0nxfqd6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pygments))
    (home-page "https://alabaster.readthedocs.io/")
    (synopsis "Configurable sidebar-enabled Sphinx theme")
    (description "Alabaster is a visually (c)lean, responsive, configurable
theme for the Sphinx documentation system.  It's the default theme of Sphinx.")
    (license license:bsd-3)))

(define-public python-sphinx-argparse
  (package
    (name "python-sphinx-argparse")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-argparse" version))
       (sha256
        (base32 "07nw68nrbpzsswb5bz8gdb5allgj6jnz8m81afhr9v6c8fyiq5c2"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sphinx))
    (native-inputs
     (list python-commonmark python-pytest python-sphinx-rtd-theme))
    (home-page "https://github.com/ribozz/sphinx-argparse")
    (synopsis "Sphinx extension for documenting argparse commands and options")
    (description
     "This package is a sphinx extension that automatically documents
argparse commands and options")
    (license license:expat)))

;;; FIXME: Currently broken by Jinja >= 3.10 (see:
;;; https://foss.heptapod.net/doc-utils/cloud_sptheme/-/issues/47).
(define-public python-sphinx-cloud-sptheme
  (package
    (name "python-sphinx-cloud-sptheme")
    (version "1.10.1")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://foss.heptapod.net/doc-utils/cloud_sptheme")
                    (changeset version)))
              (file-name (hg-file-name name version))
              (sha256
               (base32
                "0k0pgi0vcn8vdy3k6x11fpp4mqp7p3l6n6pjfi3mir3vwjhdfz7l"))))
    (build-system python-build-system)
    (native-inputs (list python-mock))
    (propagated-inputs (list python-sphinx))
    (home-page "https://foss.heptapod.net/doc-utils/cloud_sptheme")
    (synopsis "Cloud theme for Sphinx")
    (description "This package contains the @emph{Cloud} theme for Sphinx and
some related extensions.")
    (license license:bsd-3)))

(define-public python-guzzle-sphinx-theme
  (package
    (name "python-guzzle-sphinx-theme")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "guzzle_sphinx_theme" version))
        (sha256
         (base32
          "1rnkzrrsbnifn3vsb4pfaia3nlvgvw6ndpxp7lzjrh23qcwid34v"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sphinx))
    (home-page "https://github.com/guzzle/guzzle_sphinx_theme")
    (synopsis "Sphinx theme used by Guzzle")
    (description "This package provides guzzle_sphinx_theme, a theme for the
Sphinx documentation system, used by @uref{http://docs.guzzlephp.org, Guzzle}
and several other projects.")
    (license license:expat)))

(define-public python-mpl-sphinx-theme
  (package
    (name "python-mpl-sphinx-theme")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpl_sphinx_theme" version))
       (sha256
        (base32 "0ilsw6s5hfvjzqs3258c8gmg5v3dwa6k69mwmkxsyh1qmv15krpw"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pydata-sphinx-theme))
    (home-page "https://github.com/matplotlib/mpl-sphinx-theme")
    (synopsis "Matplotlib theme for Sphinx")
    (description "This package provides a Matplotlib theme for Sphinx.")
    (license license:bsd-3)))

(define-public python-sphinx-rtd-theme
  (package
    (name "python-sphinx-rtd-theme")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_rtd_theme" version))
       (sha256
        (base32
         "0p3abj91c3l72ajj5jwblscsdf1jflrnn0djx2h5y6f2wjbx9ipf"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs (list python-docutils python-sphinx))
    (home-page "https://github.com/snide/sphinx_rtd_theme/")
    (synopsis "ReadTheDocs.org theme for Sphinx")
    (description "A theme for Sphinx used by ReadTheDocs.org.")
    (license license:expat)))

(define-public python-breathe
  (package
    (name "python-breathe")
    (version "4.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "breathe" version))
       (sha256
        (base32
         "18fvphs1cb2cns9q82195fx7lmlwfikzwa10cczavpaax2jnh1xc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-docutils python-sphinx))
    (home-page "https://github.com/michaeljones/breathe")
    (synopsis "ReStructuredText and Sphinx bridge to Doxygen")
    (description "This package is an extension to reStructuredText and Sphinx
to be able to read and render the Doxygen xml output.")
    (license license:bsd-3)))

(define-public python-sphinx-intl
  (package
    (name "python-sphinx-intl")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-intl" version))
       (sha256
        (base32 "1d1q0sanjp4nkfvhsxi75zf3xjyyi8nzxvl3v7l0jy9ld70nwnmj"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sphinx python-click))
    (home-page "https://github.com/sphinx-doc/sphinx-intl")
    (synopsis
     "Sphinx utility that makes it easy to translate and to apply translation")
    (description
     "A utility tool that provides several features that make it easy to
translate and to apply translation to Sphinx generated document.")
    (license license:bsd-2)))

(define-public python-sphinxext-opengraph
  (package
    (name "python-sphinxext-opengraph")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/wpilibsuite/sphinxext-opengraph")
             (commit (string-append "v"  version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wrgpan9z65fv4hbvisz4sypc4w5ammnxkyn5lhr43wdr6b967k1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (native-inputs (list python-beautifulsoup4 python-pytest python-sphinx))
    (home-page "https://github.com/wpilibsuite/sphinxext-opengraph")
    (synopsis "Sphinx Extension to enable OpenGraph support")
    (description
     "This package provides a Sphinx Extension to generate OG metadata.")
    (license license:bsd-3)))

(define-public python-sphinx-autobuild
  (package
    (name "python-sphinx-autobuild")
    (version "2021.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-autobuild" version))
       (sha256
        (base32
         "019z8kvnaw11r41b6pfdy9iz4iwyr0s51hs0a5djn797dsva676y"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (propagated-inputs
     (list python-colorama python-livereload python-sphinx))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/GaretJax/sphinx-autobuild")
    (synopsis "Rebuild Sphinx documentation when a change is detected")
    (description
     "This package lets you watch a Sphinx directory and rebuild the
documentation when a change is detected.  It also includes a livereload
enabled web server.")
    (license license:expat)))

(define-public python-sphinx-autodoc-typehints
  (package
    (name "python-sphinx-autodoc-typehints")
    (version "1.18.3")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/tox-dev/sphinx-autodoc-typehints")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "049dlay21f4bccig31fkbzq2m8v0h6g63p1cn3dxay9q3h0mzgs0"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set to
            ;; '0.0.0'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests"
                        ;; This test requires to download an objects.inv file
                        ;; from the Sphinx website.
                        "-k" "not test_format_annotation")))))))
    (propagated-inputs (list python-sphinx))
    (native-inputs
     (list python-nptyping
           python-pytest
           python-setuptools-scm
           python-sphobjinv
           python-typing-extensions))
    (home-page "https://pypi.org/project/sphinx-autodoc-typehints/")
    (synopsis "Type hints for the Sphinx autodoc extension")
    (description "This extension allows you to use Python 3 annotations for
documenting acceptable argument types and return value types of functions.")
    (license license:expat)))

(define-public python-nbsphinx
  (package
    (name "python-nbsphinx")
    (version "0.8.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbsphinx" version))
        (sha256
          (base32
            "1v1lzkfx2lslhslqb110zxmm4dmdg6hs2rahf713c2rk9f10q2dm"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-docutils
            python-jinja2
            python-nbconvert
            python-nbformat
            python-sphinx
            python-traitlets))
    (home-page "https://nbsphinx.readthedocs.io/")
    (synopsis "Jupyter Notebook Tools for Sphinx")
    (description "@code{python-nbsphinx} is a Sphinx extension that
provides a source parser for @code{*.ipynb} files.  Custom Sphinx
directives are used to show Jupyter Notebook code cells (and of course
their results) in both HTML and LaTeX output.  Un-evaluated notebooks
- i.e. notebooks without stored output cells - will be automatically
executed during the Sphinx build process.")
    (license license:expat)))

(define-public python-sphobjinv
  (package
    (name "python-sphobjinv")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphobjinv" version))
       (sha256
        (base32
         "126lgm54c94ay3fci512ap4l607gak90pbz0fk98syxvj5izrrzx"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs python-certifi python-fuzzywuzzy
           python-jsonschema python-levenshtein))
    (home-page "https://github.com/bskinn/sphobjinv")
    (synopsis "Sphinx cross-reference tool")
    (description "Sphinx objects.inv inspection/manipulation tool.")
    (license license:expat)))

(define-public python-jupyter-sphinx
  (package
    (name "python-jupyter-sphinx")
    (version "0.3.2")
    (source
     (origin
       ;; Pypi tarball doesn't contain tests.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jupyter/jupyter-sphinx")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bsb17vzbgvrzvh87pi88b157hyigdwnf1lhrgvan03i2300h15c"))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "pytest")))))))
    (propagated-inputs
     (list python-ipython python-ipywidgets python-nbconvert
           python-nbformat))
    (native-inputs
     (list python-pytest python-sphinx))
    (home-page "https://github.com/jupyter/jupyter-sphinx/")
    (synopsis "Jupyter Sphinx Extensions")
    (description
     "Jupyter-sphinx is a Sphinx extension that executes embedded code in a
Jupyter kernel, and embeds outputs of that code in the document.  It has
support for rich output such as images, LaTeX math and even JavaScript
widgets, and supports thebelab for live code execution with minimal effort.")
    (license license:bsd-3)))

(define-public python-sphinxcontrib-autoprogram
  (package
    (name "python-sphinxcontrib-autoprogram")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-autoprogram" version))
       (sha256
        (base32
         "06hzim0d3fd72kf30fyjbbm5n8ibyybic0kf62gm79qp50zjwr5w"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list python-sphinx))
    (home-page "https://github.com/sphinx-contrib/autoprogram")
    (synopsis "Documenting CLI programs")
    (description
     "This Sphinx extension, @code{sphinxcontrib.autoprogram}, provides an
automated way to document command-line programs.  It scans
@code{argparse.ArgumentParser} object, and then expands it into a set of
@code{.. program::} and @code{.. option::} directives.")
    (license license:bsd-2)))

(define-public python-sphinx-theme-builder
  (package
    (name "python-sphinx-theme-builder")
    (version "0.2.0b1")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/pradyunsg/sphinx-theme-builder")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15gvwzd4l3wcmd6fns8xvv44yzxmamr1nfn28mp12sdw2y10v2ba"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: PEP 517 manual build copied from python-isort.
          (replace 'build
            (lambda _
              ;; ZIP does not support timestamps before 1980.
              (setenv "SOURCE_DATE_EPOCH" "315532800")
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv"))))
          (replace 'install
            (lambda _
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl)))))))
    (native-inputs (list python-flit-core python-pytest))
    (propagated-inputs
     (list python-pypa-build
           python-click
           python-nodeenv
           python-packaging
           python-pyproject-metadata
           python-rich
           python-sphinx-autobuild
           python-tomli))
    (home-page "https://github.com/pradyunsg/sphinx-theme-builder")
    (synopsis "Tool for authoring Sphinx themes")
    (description "This package provides a tool for authoring Sphinx themes
with a simple (opinionated) workflow.")
    (license license:expat)))

(define-public python-sphinx-sitemap
  (package
    (name "python-sphinx-sitemap")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-sitemap" version))
       (sha256
        (base32 "0dvpryrz7vn8rvayzy5nrmqy4wyzlaxcx88bl46prc9w4cwxmbb5"))))
    (build-system python-build-system)
    (propagated-inputs (list python-sphinx))
    (home-page "https://github.com/jdillard/sphinx-sitemap")
    (synopsis "Sitemap generator for Sphinx")
    (description "A Sphinx extension to generate multiversion and
multilanguage sitemaps.org compliant sitemaps for the HTML version of your
Sphinx documentation.")
    (license license:expat)))

(define-public python-pydata-sphinx-theme
  (package
    (name "python-pydata-sphinx-theme")
    ;; TODO: This is not the latest release, but the 0.8.x series introduced a
    ;; new Sphinx theme build system that complicate things (see:
    ;; https://github.com/pydata/pydata-sphinx-theme/issues/628 and
    ;; https://src.fedoraproject.org/rpms/python-pydata-sphinx-theme
    ;; /blob/rawhide/f/prepare_vendor.sh).
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydata-sphinx-theme" version))
       (sha256
        (base32
         "0ph69bnnw9w8vksc7rk45q5yknsrsgk9a19xsbxym46jrmgz67b7"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (propagated-inputs
     (list python-beautifulsoup4
           python-docutils
           python-jinja2
           python-sphinx))
    (native-inputs (list python-pytest python-pytest-regressions))
    (home-page "https://github.com/pydata/pydata-sphinx-theme")
    (synopsis "Bootstrap-based Sphinx theme")
    (description
     "This package provides a Bootstrap-based Sphinx theme from the PyData
community.")
    (license license:bsd-3)))
