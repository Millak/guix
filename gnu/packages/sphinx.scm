;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2019, 2020, 2021, 2023, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016-2019, 2022, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021, 2023, 2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021, 2022, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2021, 2022, 2024, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages xml))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "7.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx" version))
       (sha256
        (base32 "1zlwxv9fmpypja8w1a8lj5lrzflh710ia9nwdx05nv3yxakr4br4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; XXX: Assertions fail in these tests, check why.
                    (list "not test_additional_targets_should_be_translated"
                          "test_additional_targets_should_not_be_translated"
                          "test_customize_system_message"
                          "test_html_code_role"
                          "test_html_sidebar"
                          "test_latex_code_role"
                          "test_linenothreshold"
                          "test_literal_include_linenos"
                          "test_viewcode"
                          "test_viewcode_linenos")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; for test_cython
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-babel
           python-docutils
           python-imagesize
           python-jinja2
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
           texlive-anyfontsize
           texlive-booktabs
           texlive-capt-of
           texlive-carlisle             ;remreset
           texlive-cmap
           texlive-etoolbox
           texlive-fancyhdr
           texlive-fancyvrb
           texlive-float
           texlive-fncychap
           texlive-framed
           texlive-geometry
           texlive-hyperref
           texlive-kvoptions
           texlive-latex-bin
           texlive-ltxcmds
           texlive-needspace
           texlive-oberdiek             ;hypcap
           texlive-parskip
           texlive-preview
           texlive-tabulary
           texlive-titlesec
           texlive-tools                ;multicol, longtable
           texlive-upquote
           texlive-varwidth
           texlive-wrapfig
           texlive-xcolor))
    (native-inputs
     (list imagemagick                  ;for "convert"
           nss-certs-for-test
           python-cython
           python-defusedxml
           python-flit-core
           python-pytest
           python-pytest-xdist
           (texlive-local-tree
            (list texlive-anyfontsize texlive-cm-super texlive-tex-gyre))))
    (home-page "https://www.sphinx-doc.org")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license license:bsd-2)))

(define-public python-sphinx-6
  (package
    (inherit python-sphinx)
    (version "6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "0sycp5qx7py75fvmjz0av5awfdlqn72azzjj07x9yx5vjx3a6mkd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; 1818 passed, 24 skipped, 97 warnings
                    ;;
                    ;; AttributeError: module 'alabaster' has no
                    ;; attribute 'version'
                    (list "not test_theme_api"
                          ;; The alabaster extension used by this project
                          ;; needs at least Sphinx v3.4; it therefore cannot
                          ;; be built with this version.
                          "test_needs_sphinx"
                          ;; Various assertion errors.
                          "test_additional_targets_should_be_translated"
                          "test_additional_targets_should_not_be_translated"
                          "test_autodoc_default_options"
                          "test_html_code_role"
                          "test_latex_code_role"
                          "test_latex_images"
                          "test_linenothreshold"
                          "test_literal_include_linenos"
                          "test_viewcode")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; for test_cython
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-babel
           python-colorama
           python-docutils-0.19
           python-filelock
           python-html5lib
           python-imagesize
           python-importlib-metadata
           python-jinja2
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
           python-sphinxcontrib-websupport
           python-types-requests

           ;; The Sphinx LaTeX library '\RequirePackage' or \\usepackage
           ;; these:
           texlive-amsfonts             ;amsmath, amssymb, amstext
           texlive-amsmath
           texlive-anyfontsize
           texlive-booktabs
           texlive-capt-of
           texlive-carlisle             ;remreset
           texlive-cmap
           texlive-etoolbox
           texlive-fancyhdr
           texlive-fancyvrb
           texlive-float
           texlive-fncychap
           texlive-framed
           texlive-geometry
           texlive-hyperref
           texlive-kvoptions
           texlive-latex-bin
           texlive-ltxcmds
           texlive-needspace
           texlive-oberdiek             ;hypcap
           texlive-parskip
           texlive-preview
           texlive-tabulary
           texlive-titlesec
           texlive-tools                ;multicol, longtable
           texlive-upquote
           texlive-varwidth
           texlive-wrapfig
           texlive-xcolor))
    (native-inputs
     (list imagemagick                  ;for "convert"
           nss-certs-for-test
           python-cython
           python-flit-core
           python-pytest
           python-pytest-xdist
           (texlive-local-tree
            (list texlive-anyfontsize texlive-cm-super texlive-tex-gyre))))))

(define-public python-sphinxcontrib-apidoc
  (package
    (name "python-sphinxcontrib-apidoc")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_apidoc" version))
       (sha256
        (base32
         "13fqkbs8lwrm39lv9pmq436x3fzgr26d2svs2a7g9239sq89i6rj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pbr
           python-sphinx))
    (home-page "https://github.com/sphinx-contrib/apidoc")
    (synopsis "Sphinx extension for running @code{sphinx-apidoc}")
    (description "This package provides Sphinx extension for running
@code{sphinx-apidoc} on each build.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-applehelp
  (package
    (name "python-sphinxcontrib-applehelp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_applehelp" version))
       (sha256
        (base32 "1l863hp1pikrn04082f6jh49fha910zqfd27za79bkim2wryya9g"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-applehelp")
    (synopsis "Sphinx extension for creating Apple help books")
    (description
     "@code{sphinxcontrib-applehelp} is a Sphinx extension which outputs
Apple help books.")
    (license license:bsd-2)))

(define-public python-sphinx-basic-ng
  (package
    (name "python-sphinx-basic-ng")
    (version "1.0.0b2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinx_basic_ng" version))
              (sha256
               (base32
                "1jaihs22d8jfvk1fnv5j7hcza89hxj979ib0b4mh130cr53mmicy"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-sphinx))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/pradyunsg/sphinx-basic-ng")
    (synopsis "Modernised skeleton for Sphinx themes")
    (description
     "This package provides a modern skeleton for Sphinx themes.")
    (license license:expat)))

(define-public python-sphinx-click
  (package
    (name "python-sphinx-click")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-click" version))
       (sha256
        (base32
         "0ns6mfiw4q6g0kh11dfyzpn0rkjq9v4f3w8ry0pn5in03lr69mpm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build-system
            (lambda _
              ;; The build system is confused about this top level directory,
              ;; so we delete it.
              (delete-file-recursively "releasenotes"))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-click python-docutils python-sphinx))
    (home-page "https://github.com/click-contrib/sphinx-click")
    (synopsis "Sphinx extension that documents click applications")
    (description "This package provide sphinx extension that automatically
documents click applications.")
    (license license:expat)))

(define-public python-sphinx-copybutton
  (package
    (name "python-sphinx-copybutton")
    (version "0.5.2")
    ;; XXX: PyPI bundles <https://github.com/zenorocha/clipboard.js>.
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-copybutton" version))
       (sha256
        (base32 "1g8zxq1l258kk7yja4j2iifn0frsh60c4am9kjyd2ilnzf17rwac"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;no tests in PyPI, there are in Git
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-sphinx))
    (home-page "https://github.com/executablebooks/sphinx-copybutton")
    (synopsis "Sphinx extension to add \"copy\" buttons to code blocks")
    (description
     "This package provides a small sphinx extension to add \"copy\" buttons
to code blocks.")
    (license license:expat)))

(define-public python-sphinx-design
  (package
    (name "python-sphinx-design")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/executablebooks/sphinx-design")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n3bxibg9p16i3c3l0w8j0aw9pi9dggz1ixllgrmd9d5hdn6kl57"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core
           python-pytest
           python-pytest-cov
           python-pytest-regressions))
    (propagated-inputs
     (list python-sphinx))
    (home-page "https://sphinx-design.readthedocs.io/en/furo-theme/")
    (synopsis "Sphinx extension to designing responsive web components")
    (description
     "This package provides a sphinx extension for designing beautiful, view
size responsive web components.")
    (license license:expat)))

(define-public python-sphinxcontrib-devhelp
  (package
    (name "python-sphinxcontrib-devhelp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_devhelp" version))
       (sha256
        (base32 "1bfi9m5hg7p5vgkkqyawvdwyqj22gcvk68fmnlxxgla5sjb5s7s1"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-flit-core))
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
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))    ;no tests
    (native-inputs
     (list python-flit-core))
    (propagated-inputs
     (list python-docutils
           python-sphinx))
    (home-page "https://github.com/jupyter/sphinxcontrib_github_alt")
    (synopsis "Link to GitHub pages from Sphinx docs")
    (description
     "This package lets you link to GitHub issues, pull requests, commits and
users from Sphinx docs.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-htmlhelp
  (package
    (name "python-sphinxcontrib-htmlhelp")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-htmlhelp" version))
       (sha256
        (base32 "1sc2f368bacz4jak5kxr4n82nzri4bp37lm02g669bcarrm93qn9"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-flit-core))
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
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-jsmath" version))
       (sha256
        (base32 "1f64w19j33sp151jimibraw6qrbhd5gxy8hs3797w9478m55x4m9"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-setuptools))
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-jsmath")
    (synopsis "Sphinx extension to render math equations")
    (description
     "@code{sphinxcontrib-jsmath} is a Sphinx extension which renders display
math in HTML via JavaScript.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-jquery
  (package
    (name "python-sphinxcontrib-jquery")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-jquery" version))
       (sha256
        (base32 "0ymw7a9nahq7xn69dw8v6l3zvcj9zlnil4qskxvjqsp30jgp680n"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-sphinx))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/sphinx-contrib/jquery")
    (synopsis "Extension to include jQuery on newer Sphinx releases")
    (description
     "This package provide an extension to include @code{jQuery} on newer
Sphinx releases.")
    (license license:bsd-0)))

(define-public python-sphinxcontrib-mermaid
  (package
    (name "python-sphinxcontrib-mermaid")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
              (url "https://github.com/mgaitan/sphinxcontrib-mermaid")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cs5qcb48aigxba0g8va7xszq68d8fq2dlb843nbia3ns1p9zv9q"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyyaml python-sphinx))
    (native-inputs
     (list python-myst-parser
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/mgaitan/sphinxcontrib-mermaid")
    (synopsis "Sphinx extension for drawing Mermaid diagrams")
    (description "This extension makes it possible to draw Mermaid diagrams in
Sphinx documentation.")
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

(define-public python-sphinx-inline-tabs
  (package
    (name "python-sphinx-inline-tabs")
    (version "2023.4.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_inline_tabs" version))
       (sha256
        (base32 "1g5yhdk208i8maippnbnijd1knpai809wl3cbwzqy59cc0zz3wjx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests in the release, only in the main branch
    (native-inputs (list python-flit-core
                         python-sphinx))
    (home-page "https://github.com/pradyunsg/sphinx-inline-tabs")
    (synopsis "Add inline tabbed content to your Sphinx documentation")
    (description "This package provides a Sphinx plugin to add inline tabbed
content to your Sphinx documentation.")
    (license license:expat)))

(define-public python-sphinx-issues
  (package
    (name "python-sphinx-issues")
    (version "4.0.0")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sloria/sphinx-issues")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q4as8gibvin0n6h5y1q4cwz3b1nwgs0idfc94dbndx42pjiz1vn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'patch-sphinx-build-path
                     (lambda _
                       ;; The path to the sphinx-build binary is hardcoded to
                       ;; be in the same directory as the python
                       ;; executable. That does not work when building the
                       ;; package.
                       (substitute* "tests/test_sphinx_issues.py"
                         (((string-append "Path\\(sys\\.executable\\)"
                                          "\\.parent\\.joinpath\\"
                                          "(\"sphinx-build\"\\)"))
                          (string-append "\""
                                         #$(this-package-native-input
                                            "python-sphinx")
                                         "/bin/sphinx-build\""))))))))
    (native-inputs (list python-flit-core python-pytest python-sphinx))
    (home-page "https://github.com/sloria/sphinx-issues")
    (synopsis "Sphinx extension for linking to a project's issue tracker")
    (description
     "This package provides a Sphinx extension for linking to a project's
issue tracker.  This includes roles for linking to issues, pull requests and
user profiles.  Support for GitHub is built-in, but other services can also be
supported with @code{sphinx-issues}.")
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
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/OpenNTI/sphinxcontrib-programoutput")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02hpx6jnsx0cb1d1kk56gpj69x51m2d0prwwhsyhpwv257s64kz3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'cleanup
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (delete-file-recursively
               (string-append (site-packages inputs outputs)
                              "/sphinxcontrib/programoutput/tests"))
              (delete-file "src/sphinxcontrib/programoutput/__init__.py"))))))
    (propagated-inputs (list python-sphinx))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/OpenNTI/sphinxcontrib-programoutput")
    (synopsis "Sphinx extension to include program output")
    (description "A Sphinx extension to literally insert the output of arbitrary
commands into documents, helping you to keep your command examples up to date.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-qthelp
  (package
    (name "python-sphinxcontrib-qthelp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_qthelp" version))
       (sha256
        (base32 "1axvp4bcw3p4n8xiby9i4xlgh4znm3ia7ar3wrdh8wf1iynd1rsg"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-flit-core))
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-qthelp")
    (synopsis "Sphinx extension to output QtHelp documents")
    (description
     "@code{sphinxcontrib-qthelp} is a Sphinx extension which outputs QtHelp
documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-serializinghtml
  (package
    (name "python-sphinxcontrib-serializinghtml")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_serializinghtml" version))
       (sha256
        (base32 "0k8x0dq8p3kskwi8fg6jgwzpqchb33r0wgx52y804b47gy115ng9"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (native-inputs
     (list python-flit-core))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_websupport" version))
       (sha256
        (base32 "1kbr7q1iwmz5pdp75z15smbbg8flsk2ahap4jwglnif6pb9nfwqb"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))    ;XXX: circular dependency on Sphinx@6
    (native-inputs
     (list python-flit-core))
    (propagated-inputs
     (list python-sphinxcontrib-serializinghtml))
    (home-page "https://sphinx-doc.org/")
    (synopsis "Sphinx API for web applications")
    (description
     "This package provides a Python API to easily integrate Sphinx
documentation into your web application.  It provides tools to integrate
Sphinx documents in web templates and to handle searches.")
    (license license:bsd-3)))

(define-public python-sphinx-gallery
  (package
    (name "python-sphinx-gallery")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_gallery" version))
       (sha256
        (base32 "1crfmzl61pj308nj5q4vb98d09gpcw3bmz8jlql2wr5d819cn044"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; sphinx.errors.ConfigError: 'matplotlib_animations'
                    ;; specifies file format: mp4; this requires the
                    ;; sphinxcontrib.video package.
                    (list "not test_dummy_image"
                          ;; urllib.error.URLError: <urlopen error [Errno -3]
                          ;; Temporary failure in name resolution>
                          "test_embed_code_links_get_data")
                    " and not "))))
    (native-inputs
     (list python-absl-py
           python-graphviz
           python-ipython
           python-joblib
           python-lxml
           ;; python-intersphinx-registry ; not packaged yet
           python-matplotlib
           python-numpy
           python-packaging
           python-plotly
           python-pytest
           python-pytest-cov
           python-seaborn
           python-setuptools
           python-setuptools-scm
           ;; python-sphinxcontrib-video ; not packaged yet
           ;; python-statsmodels         ;
           python-wheel))
    (propagated-inputs
     (list python-jupyterlite-sphinx
           python-pillow
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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "repoze.sphinx.autointerface" version))
       (sha256
        (base32 "18981v34cyw8r6q19syx2vy6yjbc6afyz3287qavk5j6791g2ss8"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-sphinx
           python-zope-interface))
    (home-page "https://github.com/repoze/repoze.sphinx.autointerface")
    (synopsis "Auto-generate Sphinx API docs from Zope interfaces")
    (description "This package defines an extension for the Sphinx documentation
system.  The extension allows generation of API documentation by
introspection of @code{zope.interface} instances in code.")
    (license license:repoze)))

(define-public python-sphinx-prompt
  (package
    (name "python-sphinx-prompt")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_prompt" version))
       (sha256
        (base32 "18cmx6d5582jdaq32vnl1slfkm2zhr0raz8nkc57ikkd8rnkq6s7"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;no tests in pypi sdist
    (native-inputs (list python-poetry-core))
    (propagated-inputs
     (list python-certifi
           python-docutils
           python-idna
           python-pygments
           python-sphinx
           python-urllib3))
    (home-page "https://github.com/sbrunner/sphinx-prompt")
    (synopsis "Sphinx directive to add unselectable prompt")
    (description
     "This package provides a Sphinx directive to add unselectable prompt.")
    (license license:bsd-3)))

(define-public python-sphinx-alabaster-theme
  (package
    (name "python-sphinx-alabaster-theme")
    (version "0.7.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "alabaster" version))
       (sha256
        (base32 "0rcdsl333jjwrb8m77nsj8wdn78jg92dvk7qsw6xbnm552fbka3m"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests provided in PyPI or Git
    (propagated-inputs
     (list python-pygments))
    (native-inputs (list python-flit-core))
    (home-page "https://alabaster.readthedocs.io/")
    (synopsis "Configurable sidebar-enabled Sphinx theme")
    (description
     "Alabaster is a visually (c)lean, responsive, configurable theme for the
Sphinx documentation system.  It's the default theme of Sphinx.")
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

(define-public python-myst-parser
  (package
    (name "python-myst-parser")
    (version "4.0.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/executablebooks/MyST-Parser")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p862663sqbywlg7yavq6716wzbc05l0ddj2yhvrdf1k7sxxmypw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; "Currently only dot format is supported."
      #~(list "--ignore=tests/test_renderers/test_parse_directives.py"
              ;; AssertionError: FILES DIFFER:
              "--deselect=tests/test_sphinx/test_sphinx_builds.py::test_includes")))
    (native-inputs
     (list python-beautifulsoup4
           python-flit-core
           python-pytest
           python-pytest-param-files
           python-pytest-regressions
           python-sphinx-pytest))
    (propagated-inputs
     (list python-docutils
           python-jinja2
           python-linkify-it-py
           python-markdown-it-py
           python-linkify-it-py
           python-mdit-py-plugins
           python-pyyaml
           python-sphinx
           python-typing-extensions))
    (home-page "https://myst-parser.readthedocs.io/en/latest/")
    (synopsis "Sphinx and Docutils extension to parse MyST")
    (description "This package provides a Sphinx and Docutils extension to parse
MyST, a rich and extensible flavour of Markdown for authoring technical and
scientific documentation.")
    (license license:expat)))

(define-public python-sphinx-rtd-theme
  (package
    (name "python-sphinx-rtd-theme")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_rtd_theme" version))
       (sha256
        (base32 "11azdqbkxcibxgl3x852c2mrx19wjnwp19l6n0h3nwnsbp17nidp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-docutils
           python-sphinx
           python-sphinxcontrib-jquery))
    (home-page "https://github.com/snide/sphinx_rtd_theme/")
    (synopsis "ReadTheDocs.org theme for Sphinx")
    (description "A theme for Sphinx used by ReadTheDocs.org.")
    (license license:expat)))

(define-public python-breathe
  (package
    (name "python-breathe")
    (version "4.36.0")
    (source (origin
              (method git-fetch) ;git repo has tests
              (uri (git-reference
                    (url "https://github.com/breathe-doc/breathe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jzljqiqyb0jdndakyc69l0as1hhp23ipsbqk4i2giknypqbi8ph"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-docutils
           python-sphinx))
    (home-page "https://www.breathe-doc.org")
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

(define-public python-sphinxext-rediraffe
  (package
    (name "python-sphinxext-rediraffe")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxext-rediraffe" version))
       (sha256
        (base32 "0pbjkwmqc8q08bsk66panvpya831ycjq1ysdagyrznpzwpxcn7b5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "setup.py"
                (("version = \"main\"")
                 (string-append "version = \"" #$version "\""))))))))
    (propagated-inputs (list python-sphinx))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/wpilibsuite/sphinxext-rediraffe")
    (synopsis
     "Sphinx Extension that redirects non-existent pages to working pages")
    (description
     "This sphinx extension redirects non-existent pages to working
pages. Rediraffe can also check that deleted/renamed files in your git repo
are redirected.")
    (license license:expat)))

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
    (version "1.25.3")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/tox-dev/sphinx-autodoc-typehints")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pw9dzxrq67m0x92c0v4zqmf8llkaiw2j2plqj6n7kcravg26n6v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              ;; This test requires to download an objects.inv file
              ;; from the Sphinx website.
              (string-append "not test_format_annotation"
                             ;; XXX: Trailing -- missing.
                             " and not test_always_document_param_types"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set to
            ;; '0.0.0'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list nss-certs-for-test
           python-hatch-vcs
           python-hatchling
           python-nptyping
           python-pytest
           python-setuptools-scm
           python-sphobjinv))
    (propagated-inputs
     (list python-sphinx))
    (home-page "https://pypi.org/project/sphinx-autodoc-typehints/")
    (synopsis "Type hints for the Sphinx autodoc extension")
    (description "This extension allows you to use Python 3 annotations for
documenting acceptable argument types and return value types of functions.")
    (license license:expat)))

(define-public python-sphinx-pytest
  (package
    (name "python-sphinx-pytest")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinx_pytest" version))
              (sha256
               (base32
                "0w16w7zjhb6pxv7py7q13882r58ly4s71l2lyns0wq6qkv1za9iw"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (propagated-inputs (list python-pytest python-sphinx))
    (home-page "https://github.com/chrisjsewell/sphinx-pytest")
    (synopsis "Pytest fixtures for Sphinx extensions")
    (description "This Pytest extension mainly provides some Pytest fixtures
to simulate converting some source text to Docutils @acronym{AST, Abstract
Syntax Tree} at different stages: before transforms, after transforms, etc.")
    (license license:expat)))

(define-public python-nbsphinx
  (package
    (name "python-nbsphinx")
    (version "0.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spatialaudio/nbsphinx")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s98kab9jddy4jskllqcmwr222i85a25asi78f8nf0cpqbsvy1k6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; No tests upstream.
    (propagated-inputs
      (list python-docutils
            python-jinja2
            python-nbconvert
            python-nbformat
            python-sphinx
            python-traitlets))
    (native-inputs
     (list python-setuptools python-wheel))
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
    (version "2.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphobjinv" version))
       (sha256
        (base32 "10x4g92agj6aai0lj0xpcx58zlm1zamzws1w9dczsl1izpz07ij7"))
       (patches (search-patches "python-sphobjinv-defer-ssl-import.patch"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-dictdiffer python-pytest python-pytest-check
           python-setuptools python-wheel python-sphinx python-stdio-mgr))
    (propagated-inputs
     (list python-attrs python-certifi python-jsonschema))
    (home-page "https://github.com/bskinn/sphobjinv")
    (synopsis "Sphinx cross-reference tool")
    (description "Sphinx objects.inv inspection/manipulation tool.")
    (license license:expat)))

(define-public python-jupyter-sphinx
  (package
    (name "python-jupyter-sphinx")
    (version "0.5.3")
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
         "1l8skhjir7j9jr4xdmwzj5lk5w31jn21ydpcxvgr6adgnrdbgy53"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-W" "ignore::DeprecationWarning")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'ignore-warnings
            (lambda _
              (substitute* "pyproject.toml"
                (("\"error\",") "")))))))
    (propagated-inputs
     (list python-ipython python-ipywidgets python-nbconvert
           python-nbformat))
    (native-inputs
     (list python-hatchling
           python-ipykernel
           python-pytest
           python-sphinx))
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
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-autoprogram" version))
       (sha256
        (base32
         "02pi450qml429disph075jyqwjrawrhbsjfkqvjf10yjp6fp4sas"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-sphinx))
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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Fails due to inscrutable differences in the generated HTML
      '(list "-k" "not test_logo")))
    (propagated-inputs
     (list python-beautifulsoup4
           python-docutils-0.19
           python-jinja2
           python-sphinx-6))
    (native-inputs
     (list python-pytest python-pytest-regressions
           python-setuptools python-wheel))
    (home-page "https://github.com/pydata/pydata-sphinx-theme")
    (synopsis "Bootstrap-based Sphinx theme")
    (description
     "This package provides a Bootstrap-based Sphinx theme from the PyData
community.")
    (license license:bsd-3)))

(define-public python-sphinx-autoapi
  (package
    (name "python-sphinx-autoapi")
    (version "2.0.1") ;higher versions require Sphinx >= 5.2.0
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinx-autoapi" version))
              (sha256
               (base32
                "1fmss6ihjjx22nmjzy7qiapj1f2b86gd1vycn3zg8lh8q9l7kx6d"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-sphinx python-wheel))
    (propagated-inputs
     (list python-astroid
           python-jinja2
           python-pyyaml
           python-sphinx
           python-unidecode))
    (home-page "https://github.com/readthedocs/sphinx-autoapi")
    (synopsis "Sphinx API documentation generator")
    (description
     "Sphinx AutoAPI is a Sphinx extension for generating complete
API documentation without needing to load, run, or import the project being
documented.  In contrast to the traditional Sphinx @code{autodoc}, which
requires manual authoring and uses code imports, AutoAPI finds and generates
documentation by parsing source code.")
    (license license:expat)))
