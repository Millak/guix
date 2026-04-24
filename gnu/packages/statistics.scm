;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2026 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Vicente Vera Parra <vicentemvp@gmail.com>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2017 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017–2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Alex Branham <alex.branham@gmail.com>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021, 2022, 2024 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2022 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2023 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Jonas Freimuth <jonas.freimuth@posteo.de>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Rikard Nordgren <hrn@posteo.net>
;;; Copyright © 2026 Cayetano Santos <csantosb@inventati.org>
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

(define-module (gnu packages statistics)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby-check)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public jags
  (package
    (name "jags")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mcmc-jags/JAGS/"
                                  (version-major version) ".x/Source/"
                                  "JAGS-" version ".tar.gz"))
              (sha256
               (base32
                "0aa2w4g5057vn1qjp954s2kwxfmy1h7p5yn56fyi7sz9nmaq69gr"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--with-lapack=-lopenblas"
              "--with-blas=-lopenblas")))
    (home-page "https://mcmc-jags.sourceforge.net/")
    (native-inputs
     (list gfortran openblas))
    (synopsis "Gibbs sampler")
    (description "JAGS is Just Another Gibbs Sampler.  It is a program for
analysis of Bayesian hierarchical models using Markov Chain Monte Carlo (MCMC)
simulation not wholly unlike BUGS.  JAGS was written with three aims in mind:

@enumerate
@item To have a cross-platform engine for the BUGS language;
@item To be extensible, allowing users to write their own functions,
  distributions and samplers;
@item To be a platform for experimentation with ideas in Bayesian modelling.
@end enumerate\n")
    (license license:gpl2)))

(define-public java-jdistlib
  (package
    (name "java-jdistlib")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jdistlib/jdistlib-"
                                  version "-src.jar"))
              (sha256
               (base32
                "1pkj8aahw9ydr1isbaqrkd05nvq98ik5jwwhf3yf3rky3z869v11"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jdistlib.jar"
       #:jdk ,icedtea-8
       #:tests? #f ; no dedicated test directory
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-encoding
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/jdistlib/Beta.java"
                 (("Scheff.+-Tukey") "Scheffe-Tukey")))
             #t)))))
    (propagated-inputs
     (list java-jtransforms))
    (native-inputs
     (list java-junit))
    (home-page "https://jdistlib.sourceforge.net/")
    (synopsis "Java library of statistical distributions")
    (description "JDistlib is the Java Statistical Distribution Library, a
Java package that provides routines for various statistical distributions.")
    ;; The files that were translated from R code are under GPLv2+; some files
    ;; are under the GPLv3, which is a mistake.  The author confirmed in an
    ;; email that this whole project should be under GPLv2+.
    (license license:gpl2+)))

(define-public libxls
  (package
    (name "libxls")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libxls/libxls/releases/download/"
                           "v" version "/libxls-" version ".tar.gz"))
       (sha256
        (base32 "0b327zafbwnfxj75n722z6a6zw195rs5bjmm5wskl9dml1p87yxj"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libxls/libxls")
    (synopsis "Read binary (.xls) Excel spreadsheet files")
    (description
     "libxls is a C library to read .xls spreadsheet files in the binary OLE
BIFF8 format as created by Excel 97 and later versions.  It cannot write them.

This package also provides @command{xls2csv} to export Excel files to CSV.")
    (license license:bsd-2)))

(define-public pspp
  (package
    (name "pspp")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/pspp/pspp-" version ".tar.gz"))
       (sha256
        (base32
         "1hqlxza6cbpvhcnnns047rmydrg8f991jad7dhg6m9nzgf9m1rdm"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-test-suite
                 (lambda _
                   (substitute* "tests/output/tex.at"
                     (("AT_CHECK\\(\\[LC_ALL=C.UTF-8 pspp")
                      "AT_CHECK([LC_ALL=en_US.UTF-8 pspp"))))
               (add-before 'check 'prepare-tests
                 ;; Prevent irrelevant errors that cause test output mismatches:
                 ;; ‘Fontconfig error: No writable cache directories’
                 (lambda _
                   (setenv "XDG_CACHE_HOME" (getcwd)))))))
    (inputs
     (list cairo
           gettext-minimal
           gsl
           libxml2
           pango
           readline
           gtk+
           gtksourceview-4
           spread-sheet-widget
           zlib))
    (native-inputs
     (list autoconf ;for tests
           `(,glib "bin") ;for glib-genmarshal
           (libc-utf8-locales-for-target
            (%current-system)) ;for test 1597: tex non-ascii
           perl
           pkg-config
           python-3 ;for tests
           texinfo))
    (home-page "https://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "GNU PSPP is a statistical analysis program.  It can perform
descriptive statistics, T-tests, linear regression and non-parametric tests.
It features both a graphical interface as well as command-line input.  PSPP
is designed to interoperate with Gnumeric, LibreOffice and OpenOffice.  Data
can be imported from spreadsheets, text files and database sources and it can
be output in text, PostScript, PDF or HTML.")
    (license license:gpl3+)))

(define-public python-altair
  (package
    (name "python-altair")
    (version "5.3.0")
    (source (origin
              (method git-fetch)        ; no tests in PyPI
              (uri (git-reference
                    (url "https://github.com/altair-viz/altair")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lx3pkphi36pljns6jjxhyn9fbrana8f1y6gcg4yca48nvwlfssl"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; XXX: This test file requires hard to package python-anywidgets.
           #~(list "--ignore=tests/test_jupyter_chart.py"
                   "-k" (string-join
                         (list
                          ;; these tests open an external connection.
                          "not test_from_and_to_json_roundtrip"
                          "test_render_examples_to_chart"
                          ;; introduces a circular dependency on altair-viewer.
                          "not test_save_html"
                          ;; these tests require the vl-convert vega compiler
                          "test_vegalite_compiler"
                          "test_to_dict_with_format_vega"
                          "test_to_json_with_format_vega"
                          "test_to_url"
                          "test_renderer_with_none_embed_options"
                          "test_jupyter_renderer_mimetype")
                         " and not "))))
    (propagated-inputs (list python-jinja2
                             python-jsonschema
                             python-numpy
                             python-pandas
                             python-setuptools
                             python-toolz
                             python-typing-extensions))
    (native-inputs (list python-black
                         python-hatchling
                         python-ipython
                         python-pytest
                         python-vega-datasets))
    (home-page "https://altair-viz.github.io/")
    (synopsis "Declarative statistical visualization library for Python")
    (description
     "Vega-Altair is a declarative statistical visualization library for Python.")
    (license license:expat)))

(define-public python-arviz
  (package
    (name "python-arviz")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/arviz-devs/arviz")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04l1zsr2m80avvrh73v34sp4p9fzakmgliszsww2wmv99cl5jdk7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 3384 passed, 147 skipped, 10 deselected
      #:test-flags
      #~(list "-k" (string-join
                    ;; Network access is required.
                    (list "not test_plot_ppc_transposed"
                          "test_plot_separation[kwargs0]"
                          "test_plot_separation[kwargs1]"
                          "test_plot_separation[kwargs2]"
                          "test_plot_separation[kwargs3]"
                          "test_plot_separation[kwargs4]"
                          "test_plot_trace_legend[False-False]"
                          "test_plot_trace_legend[False-True]"
                          "test_plot_trace_legend[True-False]"
                          "test_plot_trace_legend[True-True]")
                    " and not ")
              "arviz/tests/base_tests/")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'pre-check
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cloudpickle
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-h5netcdf
           python-matplotlib
           python-numpy
           python-packaging
           python-pandas
           python-scipy
           python-typing-extensions
           python-xarray
           python-xarray-einstats))
    (home-page "https://github.com/arviz-devs/arviz")
    (synopsis "Exploratory analysis of Bayesian models")
    (description
     "ArviZ is a Python package for exploratory analysis of Bayesian models.
It includes functions for posterior analysis, data storage, model checking,
comparison and diagnostics.")
    (license license:asl2.0)))

;; Update this package together with the set of recommended packages: r-boot,
;; r-class, r-cluster, r-codetools, r-foreign, r-kernsmooth, r-lattice,
;; r-mass, r-matrix, r-mgcv, r-nlme, r-nnet, r-rpart, r-spatial, r-survival.
(define r-with-tests
  (package
    (name "r-with-tests")
    (version "4.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-major version) "/R-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "09sszy5fykpl2lyzxw5rgiabb3f5l62bqamnm7g2q9d57xlv7wli"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:disallowed-references `(,(this-package-native-input "tzdata"))
      #:make-flags
      #~(list (string-append "CFLAGS=-g -O2"
                             " -Wno-error=implicit-function-declaration")
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib/R/lib")
              ;; This affects the embedded timestamp of only the core packages.
              "PKG_BUILT_STAMP=1970-01-01")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'do-not-compress-serialized-files
            (lambda _
              ;; This ensures that Guix can detect embedded store references;
              ;; see bug #28157 for details.
              (substitute* "src/library/base/makebasedb.R"
                (("compress = TRUE") "compress = FALSE"))
              (substitute* '("src/library/tools/Makefile.in"
                             "share/make/basepkg.mk"
                             "share/make/lazycomp.mk")
                (("makeLazyLoading\\(")
                 "makeLazyLoading(compress=FALSE,"))))
          (add-before 'configure 'patch-coreutils-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((uname-bin (search-input-file inputs "/bin/uname"))
                    (rm-bin (search-input-file inputs "/bin/rm")))
                (substitute* "src/scripts/R.sh.in"
                  (("uname") uname-bin))
                (substitute* "src/unix/sys-std.c"
                  (("rm -Rf ") (string-append rm-bin " -Rf ")))
                (substitute* "src/library/parallel/R/detectCores.R"
                  (("'grep")
                   (string-append "'"
                                  (search-input-file inputs "/bin/grep")))
                  (("\\| wc -l")
                   (string-append "| "
                                  (search-input-file inputs "/bin/wc")
                                  " -l"))))))
          (add-after 'unpack 'patch-tests
            (lambda _
              ;; This is needed because R is run during the check phase and
              ;; /bin/sh doesn't exist in the build container.
              (substitute* "src/unix/sys-unix.c"
                (("\"/bin/sh\"")
                 (string-append "\"" (which "sh") "\"")))
              ;; This test fails because line numbers are off by two.
              (substitute* "tests/reg-packages.R"
                (("8 <= print" m) (string-append "## " m)))))
          (add-after 'unpack 'build-reproducibly
            (lambda _
              ;; The documentation contains time stamps to demonstrate
              ;; documentation generation in different phases.
              (substitute* "src/library/tools/man/Rd2HTML.Rd"
                (("\\\\%Y-\\\\%m-\\\\%d at \\\\%H:\\\\%M:\\\\%S")
                 "(removed for reproducibility)"))

              ;; Remove timestamp from tracing environment.  This fixes
              ;; reproducibility of "methods.rd{b,x}".
              (substitute* "src/library/methods/R/trace.R"
                (("dateCreated = Sys.time\\(\\)")
                 "dateCreated = as.POSIXct(\"1970-1-1 00:00:00\", tz = \"UTC\")"))

              ;; Ensure that gzipped files are reproducible.
              (substitute* '("src/library/grDevices/Makefile.in"
                             "doc/manual/Makefile.in")
                (("R_GZIPCMD\\)" line)
                 (string-append line " -n")))

              ;; The "srcfile" procedure in "src/library/base/R/srcfile.R"
              ;; queries the mtime of a given file and records it in an object.
              ;; This is acceptable at runtime to detect stale source files,
              ;; but it destroys reproducibility at build time.

              ;; Similarly, the "srcfilecopy" procedure records the current
              ;; time.  We change both of them to respect SOURCE_DATE_EPOCH.
              (substitute* "src/library/base/R/srcfile.R"
                (("timestamp <- (timestamp.*|file.mtime.*)" _ time)
                 (string-append "timestamp <- \
as.POSIXct(if (\"\" != Sys.getenv(\"SOURCE_DATE_EPOCH\")) {\
  as.numeric(Sys.getenv(\"SOURCE_DATE_EPOCH\"))\
} else { " time "}, origin=\"1970-01-01\")\n")))

              ;; This library is installed using "install_package_description",
              ;; so we need to pass the "builtStamp" argument.
              (substitute* "src/library/tools/Makefile.in"
                (("(install_package_description\\(.*\"')\\)\"" line prefix)
                 (string-append prefix ", builtStamp='1970-01-01')\"")))

              (substitute* "src/library/Recommended/Makefile.in"
                (("INSTALL_OPTS =" m)
                 (string-append m " --built-timestamp=1970-01-01" m)))

              ;; R bundles an older version of help2man, which does not respect
              ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
              ;; because that breaks a test.
              (with-fluids ((%default-port-encoding "ISO-8859-1"))
                (substitute* "tools/help2man.pl"
                  (("my \\$date = strftime \"%B %Y\", localtime" line)
                   (string-append line " 1"))))

              ;; The "References" section of this file when converted to
              ;; package.rds is sometimes stored with a newline, sometimes with
              ;; a space.  We avoid this problem by removing the line break
              ;; that is suspected to be the culprit.
              (substitute* "src/library/methods/DESCRIPTION.in"
                (("\\(2008\\)\n") "(2008) ")
                (("  ``Software") "``Software")
                (("Data Analysis:.") "Data Analysis:\n")
                (("Programming with R") "  Programming with R"))
              (substitute* "src/library/tools/DESCRIPTION.in"
                (("codetools, methods, xml2, curl, commonmark, knitr, xfun, mathjaxr")
                 "codetools, methods, xml2, curl, commonmark,
    knitr, xfun, mathjaxr"))))
          (add-before 'build 'set-locales
            (lambda _
              (setlocale LC_ALL "C")
              (setenv "LC_ALL" "C")))
          (add-before 'configure 'set-default-pager
            ;; Set default pager to "cat", because otherwise it is "false",
            ;; making "help()" print nothing at all.
            (lambda _ (setenv "PAGER" "cat")))
          (add-before 'configure 'set-timezone
            ;; Some tests require the timezone to be set.  However, the
            ;; timezone may not just be "UTC", or else a brittle regression
            ;; test in reg-tests-1d will fail.
            ;; We also need TZ during the configure step.
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TZ" "UTC+1")
              (setenv "TZDIR"
                      (search-input-directory inputs
                                              "share/zoneinfo"))))
          (add-before 'check 'set-home
            ;; Some tests require that HOME be set.
            (lambda _ (setenv "HOME" "/tmp")))
          (add-before 'build 'use-writable-texmfvar
           ;; Use writable TEXMFVAR to generate fonts.
           (lambda _ (setenv "TEXMFVAR" "/tmp")))
          (add-after 'build 'make-info
            (lambda _ (invoke "make" "info")))
          (add-after 'build 'install-info
            (lambda _ (invoke "make" "install-info"))))
       #:configure-flags
       #~(list
          ;; We build the recommended packages here, because they are needed in
          ;; order to run the test suite.  We disable them in the r-minimal
          ;; package.
          "--with-cairo"
          "--with-blas=-lopenblas"
          "--with-libpng"
          "--with-jpeglib"
          "--with-libtiff"
          "--with-ICU"
          "--with-tcltk"
          (string-append "--with-tcl-config="
                         #$(this-package-input "tcl")
                         "/lib/tclConfig.sh")
          (string-append "--with-tk-config="
                         #$(this-package-input "tk")
                         "/lib/tkConfig.sh")
          "--enable-R-shlib"
          "--enable-BLAS-shlib"
          "--with-system-tre")))
    ;; R has some support for Java.  When the JDK is available at configure
    ;; time environment variables pointing to the JDK will be recorded under
    ;; $R_HOME/etc and ./tools/getsp.java will be compiled which is used by "R
    ;; CMD javareconf".  "R CMD javareconf" appears to only be used to update
    ;; the recorded environment variables in $R_HOME/etc.  Refer to
    ;; https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Java-support
    ;; for additional information.

    ;; As the JDK is a rather large input with only very limited effects on R,
    ;; we decided to drop it.
    (native-inputs
     (list bzip2
           perl
           pkg-config
           texinfo                      ; for building HTML manuals
           (texlive-local-tree
            (list texlive-etoolbox
                  texlive-fancyvrb
                  texlive-inconsolata
                  texlive-upquote
                  texlive-xkeyval))
           tzdata-for-tests
           xz))
    (inputs
     (list coreutils
           curl
           openblas
           gfortran
           grep
           icu4c
           libdeflate
           libjpeg-turbo
           libpng
           libtiff
           libxt
           ;; We need not only cairo here, but pango to ensure that tests for the
           ;; "cairo" bitmapType plotting backend succeed.
           pango
           pcre2
           readline
           tcl
           tk
           which
           zlib
           ;; This avoids a reference to the ungraftable static bash.  R uses the
           ;; detected shell for the "system" procedure.
           bash-minimal))
    (native-search-paths
     (list (search-path-specification
            (variable "R_LIBS_SITE")
            (files (list "site-library/")))))
    (home-page "https://www.r-project.org/")
    (synopsis "Environment for statistical computing and graphics")
    (description
     "R is a language and environment for statistical computing and graphics.
It provides a variety of statistical techniques, such as linear and nonlinear
modeling, classical statistical tests, time-series analysis, classification
and clustering.  It also provides robust support for producing
publication-quality data plots.  A large amount of 3rd-party packages are
available, greatly increasing its breadth and scope.")
    (license license:gpl3+)))

(define-public python-chaospy
  (package
    (name "python-chaospy")
    (version "4.3.21")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jonathf/chaospy")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x9jsy1jsshki0jn2hh8dby20b9haznzas2m03y3wswfgcjbr3md"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 388 passed, 33 warnings
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: The deprecation warnings break the tests, report upstream.
          ;;
          ;; AttributeError: module 'numpy' has no attribute 'bool'. `np.bool`
          ;; was a deprecated alias for the builtin `bool`. To avoid this
          ;; error in existing code, use `bool` by itself. Doing this will not
          ;; modify any behavior and is safe. If you specifically wanted the
          ;; numpy scalar type, use `np.bool_` here. The aliases was
          ;; originally deprecated in NumPy 1.20; for more details and
          ;; guidance see the original release note at:
          ;; https://numpy.org/devdocs/release/1.20.0-notes.html#deprecations
          (add-after 'unpack 'dont-treat-deprecation-warnings-as-error
            (lambda _
              (substitute* "pyproject.toml"
                (("\"error::DeprecationWarning\",") "")))))))
    (native-inputs
     (list python-pytest
           python-scikit-learn
           python-setuptools))
    (propagated-inputs
     (list python-numpoly
           python-numpy
           python-scipy))
    (home-page "https://chaospy.readthedocs.io/en/master/")
    (synopsis "Numerical tool for performing uncertainty quantification")
    (description "Chaospy is a numerical toolbox for performing uncertainty
quantification using polynomial chaos expansions, advanced Monte Carlo
methods implemented in Python.  It also include a full suite of tools for
doing low-discrepancy sampling, quadrature creation, polynomial manipulations,
and a lot more.")
    (license license:expat)))

(define-public python-dcor
  (package
    (name "python-dcor")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dcor" version))
       (sha256
        (base32 "1knbaygh489v5hz6fggdv09lz323zklqjb5m52pkkv6pjs2l0v9q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; doctests require NumPy1.
      #:test-flags #~(list "-o" "addopts=''")))
    (native-inputs
     (list python-array-api-strict
           python-pytest
           python-pytest-cov    ;test collection fails without
           python-setuptools))
    (propagated-inputs
     (list python-array-api-compat
           python-joblib
           python-numba
           python-numpy
           python-scipy))
    (home-page "https://dcor.readthedocs.io/")
    (synopsis "Distance correlation and related E-statistics in Python")
    (description
     "@code{dcor} is distance correlation and energy statistics in Python.

E-statistics are functions of distances between statistical observations in
metric spaces.  Distance covariance and distance correlation are dependency
measures between random vectors introduced in
@url{https://github.com/vnmabus/dcor#srb07,[SRB07]} with a simple E-statistic
estimator.

This package offers functions for calculating several E-statistics such as:

@itemize
@item estimator of the energy distance
@url{https://github.com/vnmabus/dcor#sr13,[SR13]}
@item biased and unbiased estimators of distance covariance and distance
correlation @url{https://github.com/vnmabus/dcor#srb07,[SRB07]}
@item estimators of the partial distance covariance and partial distance
covariance @url{https://github.com/vnmabus/dcor#sr14,[SR14]}
@end itemize")
    (license license:expat)))

(define-public python-diptest
  (package
    (name "python-diptest")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/RUrlus/diptest")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j35v849yx6bq9w4bvlgbw9b2g3f1zlqxf63vv257dfx5qsa5x62"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal ;TODO: propagate from python-scikit-build-core
           pybind11-2
           python-pytest
           python-scikit-build-core))
    (propagated-inputs
     (list python-numpy
           python-psutil))
    (home-page "https://github.com/RUrlus/diptest")
    (synopsis "Hartigan's diptest")
    (description
     "This package provides a Python/C(++) implementation of
@url{https://www.jstor.org/stable/2241144, Hartigan & Hartigan's dip test} for
unimodality.

The dip test measures multimodality in a sample by the maximum difference,
over all sample points, between the empirical distribution function, and the
unimodal distribution function that minimizes that maximum difference. Other
than unimodality, it makes no further assumptions about the form of the null
distribution.")
    (license (list license:gpl2+    ;in pyproject.toml
                   license:gpl3)))) ;in LICENSE

(define-public python-dynesty
  (package
    (name "python-dynesty")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dynesty" version))
       (sha256
        (base32 "04fkbixkfyqlr8zjky177bmqxqd19xcicqx3r1mhhy0z7942sx7x"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; To run a full tests suite takes a few hours on 16 threads,
              ;; skip slow tests.
              "-m" "not slow"
              ;; Tests fail with error: (liwork>=max(1,10*n)||liwork==-1)
              ;; failed for 10th keyword liwork: dsyevr:liwork=
              "--deselect=tests/test_ellipsoid.py::test_bounding_crazy[1]"
              "--deselect=tests/test_plot.py::test_gaussian[True-False-1-multi]")))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-h5py
           python-matplotlib
           python-numpy
           python-scipy
           python-tqdm))
    (home-page "https://github.com/joshspeagle/dynesty")
    (synopsis "Dynamic nested sampling computing Bayesian posteriors and evidences")
    (description
     "This package implements a Dynamic Nested Sampling for computing Bayesian
posteriors and evidences.")
    (license license:expat)))

(define-public r-minimal
  (package (inherit r-with-tests)
    (name "r-minimal")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:tests? #f #f) #f)
       ((#:disallowed-references refs '())
        (cons (this-package-native-input "perl") refs))
       ((#:configure-flags flags)
        ;; Do not build the recommended packages.  The build system creates
        ;; random temporary directories and embeds their names in some
        ;; package files.  We build these packages with the r-build-system
        ;; instead.
        #~(cons "--without-recommended-packages" #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'remove-extraneous-references
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (substitute* (string-append #$output "/lib/R/etc/Makeconf")
                  (("^# configure.*")
                   "# Removed to avoid extraneous references\n"))
                (substitute* (string-append #$output "/lib/R/bin/libtool")
                  (((string-append
                     "(-L)?("
                     (format #false
                             "~a/[^-]+-(~{~a~^|~})-[^/]+"
                             (%store-directory)
                             '("bzip2"
                               "file"
                               "glibc-utf8-locales"
                               "graphite2"
                               "libselinux"
                               "libsepol"
                               "perl"
                               "texinfo"
                               "texlive-bin"
                               "util-macros"
                               "xz"))
                     "|"
                     (format #false "~a/[^-]+-glibc-[^-]+-static"
                             (%store-directory))
                     ")/lib")) ""))))))))))

(define-public rmath-standalone
  (package (inherit r-minimal)
    (name "rmath-standalone")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-timezone
           ;; We need TZ during the configure step.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo"))))
         (add-after 'configure 'chdir
           (lambda _ (chdir "src/nmath/standalone/"))))))
    (synopsis "Standalone R math library")
    (description
     "This package provides the R math library as an independent package.")))

(define-public r
  (package (inherit r-minimal)
    (name "r")
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     (list r-minimal
           r-boot
           r-class
           r-cluster
           r-codetools
           r-foreign
           r-kernsmooth
           r-lattice
           r-mass
           r-matrix
           r-mgcv
           r-nlme
           r-nnet
           r-rpart
           r-spatial
           r-survival))))

(define-public r-xpose4
  (package
    (name "r-xpose4")
    (version "4.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/UUPharmacometrics/xpose4")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p1mrgb3s7iz61366v36xswn36ivpgqiy7qwxy6yh3hi6y6rjf2j"))))
    (build-system r-build-system)
    (native-inputs (list r-testthat))
    (propagated-inputs (list r-lattice
                             r-hmisc
                             r-survival
                             r-dplyr
                             r-tibble
                             r-lazyeval
                             r-gam
                             r-readr))
    (home-page "https://uupharmacometrics.github.io/xpose4/")
    (synopsis "Diagnostics for nonlinear mixed-effect models")
    (description
     "This package is a model building aid for nonlinear mixed-effects
(population) model analysis using NONMEM, facilitating data set checkout,
exploration and visualization, model diagnostics, candidate covariate
identification and model comparison.  The methods are described in
Keizer et al. (2013) <doi:10.1038/psp.2013.24>, and Jonsson et al.
(1999) <doi:10.1016/s0169-2607(98)00067-4>.")
    (license license:lgpl3+)))

(define-public python-vega-datasets
  (package
    (name "python-vega-datasets")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vega_datasets" version))
       (sha256
        (base32 "1h1zv607mars2j73v8fdwihjh479blqxyw29nhmc73lf40s9iglx"))
       (modules '((guix build utils)))
       (patches
        (search-patches "python-vega-datasets-remove-la-riots-code.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-la-riots-dataset
                 ;; Remove dataset with unclear license.
                 (lambda _
                   (delete-file "vega_datasets/_data/la-riots.csv"))))))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-pandas))
    (home-page "https://github.com/altair-viz/vega_datasets")
    (synopsis "Example datasets used by Vega-related projects")
    (description "This package provides a collection of datasets used in Vega
and Vega-Lite examples.")
    (license license:expat)))

(define-public python-emcee
  (package
    (name "python-emcee")
    (version "3.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "emcee" version))
       (sha256
        (base32 "0zb3ihja3hyj6zjbhkpxhyyppnv58q2jjg0yd1lwmydqdaplvbqi"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (home-page "https://emcee.readthedocs.io/en/stable/")
    (synopsis "Ensemble sampling toolkit for MCMC")
    (description
     "@code{emcee} is a Python implementation of the affine-invariant ensemble
sampler for Markov chain Monte Carlo (MCMC).")
    (license license:expat)))

(define-public python-george
  (package
    (name "python-george")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "george" version))
       (sha256
        (base32 "1zvbdq50ds820aj06lr2nrzwg121bkd9bg0aq83gvk7lf8yqgp4v"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal
           pybind11-2
           python-pytest
           python-scikit-build-core
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://george.readthedocs.io")
    (synopsis "Fast Gaussian Processes for regression")
    (description
     "George is a fast and flexible Python library for Gaussian Process (GP)
Regression, focused on efficiently evaluating the marginalized likelihood of a
dataset under a GP prior, even as this dataset gets Big.")
    (license license:expat)))

(define-public python-getdist
  (package
    (name "python-getdist")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "getdist" version))
       (sha256
        (base32 "01s1p53pqpxbi8sy2030jpjn7gsy67zb7y6p0gf57lgxvp4zx74q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-packaging
           python-pyyaml
           python-scipy))
    (home-page "https://github.com/cmbant/getdist")
    (synopsis "GetDist Monte Carlo sample analysis, plotting and GUI")
    (description
     "GetDist is a Python package for analysing Monte Carlo samples, including
correlated samples from Markov Chain Monte Carlo (MCMC).")
    (license license:lgpl3)))

(define-public python-hdmedians
  (package
    (name "python-hdmedians")
    (version "0.14.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hdmedians" version))
              (sha256
               (base32
                "1mn2k8srnmfy451l7zvb2l4hn9701bc5awjm6q3vmqbicyqyqyml"))
       (patches (search-patches "python-hdmedians-replace-nose.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "hdmedians")))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-cython
           python-numpy
           python-setuptools
           python-wheel))
    (home-page "http://github.com/daleroberts/hdmedians")
    (synopsis "High-dimensional medians")
    (description "Various definitions for a high-dimensional median exist and
this Python package provides a number of fast implementations of these
definitions.  Medians are extremely useful due to their high breakdown
point (up to 50% contamination) and have a number of nice applications in
machine learning, computer vision, and high-dimensional statistics.")
    (license license:asl2.0)))

(define-public python-nautilus-sampler
  (package
    (name "python-nautilus-sampler")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nautilus_sampler" version))
       (sha256
        (base32 "1b73rxg7b5fzpw4ss4py98xdxddkl1dh2dszp2pxv3y179iyniqj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--durations=0"
              ;; One Dask test hangs.
              "-k" "not test_pool[dask]")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _ (setenv "OMP_NUM_THREADS" "1"))))))
    (native-inputs
     (list python-dask
           python-distributed
           python-flit-core
           python-h5py
           python-pytest
           python-pytest-xdist))
    (propagated-inputs
     (list python-numpy
           python-scikit-learn
           python-scipy
           python-threadpoolctl))
    (home-page "https://github.com/johannesulf/nautilus")
    (synopsis "Neural Network-Boosted Importance Sampling for Bayesian Statistics")
    (description
     "Nautilus is an pure-Python package for Bayesian posterior and evidence
estimation.  It utilizes importance sampling and efficient space exploration
using neural networks.  Compared to traditional @acronym{MCMC, Markov chain
Monte Carlo} and Nested Sampling codes, it often needs fewer likelihood calls
and produces much larger posterior samples.  Additionally, nautilus is highly
accurate and produces Bayesian evidence estimates with percent precision.  It
is widely used in many areas of astrophysical research.")
    (license license:expat)))

(define-public python-nestle
  (package
    (name "python-nestle")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nestle" version))
       (sha256
        (base32 "0v94qcqwl519vqhd4wb1zhx4x4q9xhbck8g2h0v2n4mwxgz9irsx"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests in PyPI or Git
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/kbarbary/nestle")
    (synopsis "Nested sampling algorithms for evaluating Bayesian evidence")
    (description
     "This package provides an implementation of
@url{https://en.wikipedia.org/wiki/Nested_sampling_algorithm, Nested Sampling}
algorithms for evaluating Bayesian evidence.")
    (license license:expat)))

(define-public python-pymc
  (package
    (name "python-pymc")
    (version "5.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pymc-devs/pymc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n2qj13qpd58qg8s5gx2qxiiy7hjpk6k8z0a4ysr3jc66d12s092"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; tests are too computationally intensive
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'versioneer
            (lambda _
              (invoke "versioneer" "install")
              (substitute* "setup.py"
                (("version=versioneer.get_version\\(),")
                 (format #f "version=~s," #$version))))))))
    (native-inputs
     (list python-setuptools
           python-versioneer))
    (propagated-inputs
     (list python-arviz
           python-cachetools
           python-cloudpickle
           python-numpy
           python-pandas
           python-pytensor
           python-rich
           python-scipy
           python-threadpoolctl
           python-typing-extensions))
    (home-page "https://github.com/pymc-devs/pymc")
    (synopsis "Library for probabilistic programming in Python")
    (description
     "PyMC (formerly PyMC3) is a Python package for Bayesian statistical
modeling focusing on advanced Markov chain Monte Carlo (MCMC) and variational
inference (VI) algorithms.")
    (license license:asl2.0)))

(define-public python-patsy
  (package
    (name "python-patsy")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "patsy" version))
              (sha256
               (base32
                "1i60b6s8zj0w2ks63ip4mr8z14p6pixp76rm9q2qr0gc3qwsk1p7"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-numpy python-scipy))
    (native-inputs
     (list python-pytest python-pytest-cov python-setuptools python-wheel))
    (home-page "https://github.com/pydata/patsy")
    (synopsis "Describe statistical models and build design matrices")
    (description
     "Patsy is a Python package for describing statistical models and for
building design matrices.")
    ;; The majority of the code is distributed under BSD-2.  The module
    ;; patsy.compat contains code derived from the Python standard library,
    ;; and is covered by the PSFL.
    (license (list license:bsd-2 license:psfl))))

(define-public python-kalepy
  (package
    (name "python-kalepy")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kalepy" version))
       (sha256
        (base32 "1a1d98vjkjs8zwx4hdss3gv67jyf25mmsrdc5qi8hpxminkizb6w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 46 passed, 6 deselected, 214 warnings
      #:test-flags
      ;; XXX: See: <https://github.com/lzkelley/kalepy/issues/22>.
      ;; AttributeError: module 'numpy' has no attribute 'product'
      #~(list #$@(map (lambda (test) (string-append "--deselect="
                                                    "kalepy/tests/"
                                                    "test_utils.py::"
                                                    test))
                      (list "Test_Midpoints::test_midpoints_lin"
                            "Test_Midpoints::test_midpoints_log"
                            "Test_Trapz::test_nd"
                            "Test_Trapz_Dens_To_Mass::test_ndim"
                            "Test_Trapz_Dens_To_Mass::test_ndim_a2")))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-matplotlib
           python-numba
           python-numpy
           python-scipy
           python-six))         ;XXX: hard dependency
    (home-page "https://github.com/lzkelley/kalepy")
    (synopsis "Kernel Density Estimation (KDE) and sampling")
    (description
     "This package performs KDE operations on multidimensional data to
calculate estimated PDFs (probability distribution functions), and resample
new data from those PDFs.")
    ;; MIT   - setup.py
    ;; GPL3+ - LICENSE
    (license (list license:gpl3+ license:expat))))

(define-public python-lifelines
  (package
    (name "python-lifelines")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lifelines" version))
       (sha256
        (base32 "065yajlfydi7x7b1sjxp9h3rqgwrd3w9ivxiyph7y5nbbwkzdxpp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 144 passed, 75 skipped, 1 deselected, 11 warnings
      #:test-flags
      ;; Tests are not deterministic and compute intense.
      #~(list "--ignore=lifelines/tests/test_estimation.py"
              ;; Not equal to tolerance rtol=0.0001, atol=0
              "--deselect=lifelines/tests/test_npmle.py::test_mice_scipy")))
    (native-inputs
     (list python-dill
           python-flaky
           python-jinja2
           python-joblib
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-autograd
           python-autograd-gamma
           python-formulaic
           python-matplotlib
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://github.com/CamDavidsonPilon/lifelines")
    (synopsis
     "Survival analysis including Kaplan Meier, Nelson Aalen and regression")
    (description
     "This package enables survival analysis in Python, including Kaplan
Meier, Nelson Aalen and regression.")
    (license license:expat)))

(define-public python-mapie
  (package
    (name "python-mapie")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mapie" version))
              (sha256
               (base32
                "1qh7a4cm67jq7ip7j8225w44x9vv2yqgakwnnj7bhjkvcvfn1iwl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "mapie")))
    (native-inputs (list python-pandas python-pytest python-setuptools
                         python-wheel))
    (propagated-inputs (list python-numpy python-scikit-learn))
    (home-page "https://github.com/scikit-learn-contrib/MAPIE")
    (synopsis "Module for estimating prediction intervals")
    (description "MAPIE allows you to easily estimate prediction intervals
(or prediction sets) using your favourite scikit-learn-compatible model for
single-output regression or multi-class classification settings.

Prediction intervals output by MAPIE encompass both aleatoric and epistemic
uncertainties and are backed by strong theoretical guarantees thanks to
conformal prediction methods intervals.")
    (license license:bsd-3)))

(define-public python-statannotations
  (package
    (name "python-statannotations")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/trevismd/statannotations")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0433w2vv2j1kwbkgbfiv5xq1s2fyxvwz520b7jwa0a8s9ndhjpwj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 173 passed, 5 deselected, 364 warnings
      #:test-flags
      ;; Network access is required for these tests:
      #~(list #$@(map (lambda (test)
                        (string-append "--deselect=tests/"
                                       "test_native_scale.py::" test))
                      (list "TestNativeScalePlotter::test_gap"
                            "TestNativeScalePlotter::test_native_scale"
                            "TestNativeScalePlotter::test_no_native_scale"))
              #$@(map (lambda (test)
                        (string-append "--deselect=tests/"
                                       "test_positions.py::" test))
                      (list "TestPositionPlotter::test_seaborn_all_plots"
                            "TestPositionPlotter::test_seaborn_violinplot")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'pre-check
            (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-pandas
           python-scipy
           python-seaborn))
    (home-page "https://github.com/trevismd/statannotations")
    (synopsis "Add statistical significance annotations on seaborn plots")
    (description
     "This package implements a functionality to optionally compute statistical
test and add statistical annotations on plots generated with seaborn.")
    (license license:expat)))

(define-public python-statsmodels
  (package
    (name "python-statsmodels")
    (version "0.14.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statsmodels" version))
       (sha256
        (base32 "1pvd3k3jr9akfl7zk90s7a2wmmikf8smmd9mz3fwxlngric0w9ny"))
       (modules '((guix build utils)))
       (snippet
        '(for-each delete-file (find-files "." "\\.c$")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite is very large and rather brittle.  Tests often fail
      ;; because of minor changes in dependencies that upstream hasn't fixed
      ;; in a new release.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-matplotlib-backend-to-agg
            (lambda _
              ;; Set the matplotlib backend to Agg to avoid problems using the
              ;; GTK backend without a display.
              (substitute* (append (find-files "statsmodels/graphics/tests" "\\.py")
                                   '("statsmodels/tsa/vector_ar/tests/test_var.py"
                                     "statsmodels/duration/tests/test_survfunc.py"))
                (("import matplotlib\\.pyplot as plt" line)
                 (string-append "import matplotlib;matplotlib.use('Agg');"
                                line))))))))
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-pandas
           python-patsy
           python-scipy))
    (native-inputs
     (list python-cython
           python-matplotlib
           python-setuptools
           python-setuptools-scm))
    (home-page
     (string-append "https://www.statsmodels.org/v" version "/"))
    (synopsis "Statistical modeling and econometrics in Python")
    (description
     "Statsmodels is a Python package that provides a complement to scipy for
statistical computations including descriptive statistics and estimation and
inference for statistical models.")
    (license license:bsd-3)))

(define-public python-openturns
  (package
    (name "python-openturns")
    (version "1.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openturns/openturns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pkxwpdpz5bh7hipys3zg6ippzzy23qydgqjglpxc3g218xmy0cl"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          ;; This is a Python package which is fully managed by CMake.  In
          ;; cmake-build-system the check phase runs before install, but the
          ;; Python modules required for testing are only installed in the
          ;; install phase.  Move check to after the install phase.
          (add-after 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "--exclude-regex"
                        ;; XXX: Cpp tests fail in 'No such file or directory',
                        ;; skip for now and only run the Python tests.
                        ;; TODO: To pass the Python tests below, Ipopt must be
                        ;; built with MUMPS support, but simply adding mumps
                        ;; to the inputs doesn't work for it to be found,
                        ;; possibly because MUMPS doesn't generate a .pc file.
                        (string-join
                         (list "^cpp"
                               "pyinstallcheck_Bonmin_std"
                               "pyinstallcheck_Bonmin_4dsoo"
                               "pyinstallcheck_Bonmin_MIT15"
                               "pyinstallcheck_Bonmin_swiler2014"
                               "pyinstallcheck_Ipopt_std"
                               "pyinstallcheck_example_plot_optimization_bonmin"
                               "pyinstallcheck_coupling_tools"
                               ;; Subprocess aborted for these tests.
                               "pyinstallcheck_Study_std"
                               "pyinstallcheck_OptimizationAlgorithm_std"
                               "pyinstallcheck_docstring_missing"
                               ;; These tests are flaky and fail non
                               ;; reproducibly.
                               "pyinstallcheck_KrigingAlgorithm_std"
                               "pyinstallcheck_GaussianProcessFitter_std")
                         "|"))))))))
    (native-inputs
     (list bison
           flex
           python-numpydoc
           python-sphinx
           ;; python-sphinx-gallery ;; Currently broken
           swig-4.0
           texlive-dvisvgm))
    (inputs
     (list openblas                ; the only required dependency
           ;; The dependencies below are all optional.
           bonmin
           boost
           cbc ;; Maybe this should be propagated by Bonmin?
           ceres
           cminpack
           dlib
           hdf5
           hmat
           ipopt
           libxml2
           mpc
           mpfr
           nlopt
           pagmo
           primesieve
           python-wrapper
           spectra
           onetbb))
    (propagated-inputs
     (list python-chaospy
           python-dill
           python-matplotlib
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://openturns.github.io/www/")
    (synopsis "Uncertainty treatment library")
    (description
     "OpenTURNS is a scientific C++ and Python library including an internal
data model and algorithms dedicated to the treatment of uncertainties.  The
main goal of this library is giving to specific applications all the
functionalities needed to treat uncertainties in studies.")
    (license license:lgpl3+)))

(define-public python-resample
  (package
    (name "python-resample")
    (version "1.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "resample" version))
       (sha256
        (base32 "0c9ccygxqjvlklfcngk30myzqbvis8szj5nzj9c305v58pcj2656"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/scikit-hep/resample")
    (synopsis "Resampling-based inference in Python")
    (description "This package provides a resampling-based inference based on
data resampling and permutation.

Features:

@itemize
@item Bootstrap resampling: ordinary or balanced with optional stratification
@item Extended bootstrap resampling: also varies sample size
@item Parametric resampling: Gaussian, Poisson, gamma, etc.)
@item Jackknife estimates of bias and variance of any estimator
@item Compute bootstrap confidence intervals (percentile or BCa) for any
estimator
@item Permutation-based variants of traditional statistical tests (USP test of
independence and others)
@item Tools for working with empirical distributions (CDF, quantile, etc.)
@end itemize")
    (license license:bsd-3)))

(define-public python-zeus-mcmc
  (package
    (name "python-zeus-mcmc")
    (version "2.5.4")
    (source
     (origin
       (method git-fetch)        ; no tests in PyPI
       (uri (git-reference
             (url "https://github.com/minaskar/zeus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sci442fx2bkkj8169hwnx6psl7m2r8y1cicn1xjxxgqaby5j8pi"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-scikit-learn
           python-scipy
           python-seaborn
           python-setuptools
           python-tqdm))
    (home-page "https://github.com/minaskar/zeus")
    (synopsis "Deep learning energy measurement and optimization framework")
    (description
     "This package provides an implementation of the Ensemble Slice Sampling method.
Features:
@itemize
@item fast & Robust Bayesian Inference
@item efficient Markov Chain Monte Carlo (MCMC)
@item black-box inference, no hand-tuning
@item excellent performance in terms of autocorrelation time and convergence rate
@item scale to multiple CPUs without any extra effort
@item automated Convergence diagnostics
@end itemize")
    (license license:asl2.0)))

(define-public r-spams
  (package
    (name "r-spams")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.inria.fr/thoth/spams-devel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qvj87fw4sm54c7dvhxjgmgvnyyrrz9fk6dqp3ak0gwgb42gqh60"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-generated-file-shebangs 'patch-paths
            (lambda _
              (substitute* (cons* "swig/setRelease"
                                  "swig/R/docmatlab2R"
                                  "swig/R/mkdist"
                                  (find-files "." "(^mk|\\.sh$)"))
                (("/bin/pwd") "pwd")
                (("/bin/rm") "rm"))))
          (add-after 'patch-paths 'mkdist
            (lambda _
              (chdir "swig/R")
              (setenv "TEXINPUTS" (string-append (getcwd) "/../../doc:"))
              (substitute* "mkdist"
                (("^SWIG=.*")
                 (string-append "SWIG=" (which "swig")))
                (("^../mkdoc") ""))     ;requires texlive-aeguill
              (invoke "./mkdist")
              (chdir "dist/spams-R/spams")))
          ;; Don't tune for the building machine.
          (add-after 'mkdist 'no-mtune
            (lambda _
              (substitute* "src/Makevars"
                (("-mtune=native") ""))))
          (add-after 'no-mtune 'use-c++11
            (lambda _
              (substitute* "src/Makevars"
                (("-DUSE_BLAS_LIB" m)
                 (string-append m " -std=c++11"))))))))
    (native-inputs
     (list hevea
           perl
           swig-4.0
           (texlive-local-tree
            (list texlive-aeguill texlive-jknapltx))))
    (propagated-inputs
     (list r-lattice
           r-matrix))
    (home-page "https://gitlab.inria.fr/thoth/spams-devel/")
    (synopsis "Toolbox for solving sparse estimation problems")
    (description "SPAMS (SPArse Modeling Software) is an optimization toolbox
for solving various sparse estimation problems.  It includes tools for the
following problems:

@enumerate
@item Dictionary learning and matrix factorization (NMF, sparse @dfn{principle
 component analysis} (PCA), ...)
@item Solving sparse decomposition problems with LARS, coordinate descent,
 OMP, SOMP, proximal methods
@item Solving structured sparse decomposition problems (l1/l2, l1/linf, sparse
 group lasso, tree-structured regularization, structured sparsity with
 overlapping groups,...).
@end enumerate\n")
    (license license:gpl3+)))

(define-public r-tgconfig
  (let ((commit "15cf199436ae0b2ac0006b2ca7f0aeeb5c9d4445")
        (revision "1"))
    (package
      (name "r-tgconfig")
      (version (git-version "0.1.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tanaylab/tgconfig")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "159m8hhbk9ip2fdy6zsa96v0173q1awcrzdz2rr2796awfaxjgx1"))))
      (properties `((upstream-name . "tgconfig")))
      (build-system r-build-system)
      (propagated-inputs (list r-yaml))
      (native-inputs (list r-testthat))
      (home-page "https://github.com/tanaylab/tgconfig")
      (synopsis "Infrastructure for managing package parameters")
      (description
       "This is a package to provide infrastructure for managing package parameters.
Parameters are easy to get in relevant functions within a package,
and error is thrown if a parameter is missing.  Developers are able
to register parameters and set their default value in a config file
that is part of the package in YAML format, and users are able to
override parameters using their own YAML.  Users get an exception
when trying to override a parameter that was not registered, and
can load multiple parameters to the current environment.")
      (license license:gpl3+))))

(define-public r-tgutil
  (let ((commit "db4ff8b98082f8e4dbdeacb452641d215fd3c7ff")
        (revision "1"))
    (package
      (name "r-tgutil")
      (version (git-version "0.1.15" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tanaylab/tgutil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00rsqs7f896piywh84jr8fkphbbx4jb7radf6znhhj6fip63yn91"))))
      (properties `((upstream-name . "tgutil")))
      (build-system r-build-system)
      (propagated-inputs (list r-broom
                               r-cowplot
                               r-data-table
                               r-dplyr
                               r-ggplot2
                               r-glue
                               r-magrittr
                               r-matrix
                               r-matrixstats
                               r-qlcmatrix
                               r-readr
                               r-rlang
                               r-scales
                               r-tibble
                               r-tidyr))
      (native-inputs (list r-testthat))
      (home-page "https://github.com/tanaylab/tgutil")
      (synopsis "Simple utility functions for Tanay lab code")
      (description
       "This package provides simple utility functions that are shared
across several packages maintained by the Tanay lab.")
      (license license:gpl3))))

(define-public r-catterplots
  (let ((commit "ae17cd5e49ddda4ecfe0eba8a4c21df8c88e72c4")
        (revision "3"))
    (package
      (name "r-catterplots")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Gibbsdavidl/CatterPlots")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qa8liylffpxgdg8xcgjar5dsvczqhn3akd4w35113hnyg1m4xyg"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-png))
      (home-page "https://github.com/Gibbsdavidl/CatterPlots")
      (synopsis "Scatter plots with cat shaped points")
      (description "Did you ever wish you could make scatter plots with cat
shaped points?  Now you can!")
      (license license:asl2.0))))

(define-public r-nnlm
  (let ((commit "4574bca9456fe2285b668b4c22a908cffbad10a0")
        (revision "1"))
    (package
      (name "r-nnlm")
      (version (git-version "0.4.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/linxihui/NNLM")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qzvav4ch0j1k7jcqzk9cvl8dx79fapmvdzmzzyl8smscybhfgba"))))
      (properties `((upstream-name . "NNLM")))
      (build-system r-build-system)
      (propagated-inputs (list r-rcpp r-rcpparmadillo r-rcppprogress))
      (native-inputs (list r-knitr r-testthat))
      (home-page "https://github.com/linxihui/NNLM")
      (synopsis "Fast and versatile non-negative matrix factorization")
      (description
       "This is a package for @dfn{Non-Negative Linear Models} (NNLM).  It
implements fast sequential coordinate descent algorithms for non-negative
linear regression and @dfn{non-negative matrix factorization} (NMF).  It
supports mean square error and Kullback-Leibler divergence loss.  Many other
features are also implemented, including missing value imputation, domain
knowledge integration, designable W and H matrices and multiple forms of
regularizations.")
      (license license:bsd-2))))

(define-public r-languageserver
  (let ((commit "004da9388f9b19990f031c8dc9b527fb406378ba")
        (revision "1"))
    (package
      (name "r-languageserver")
      (version (git-version "0.3.12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/REditorSupport/languageserver")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "05f22bjpc87fngzq4jsk2q2yb2i3ha03b377r0wx15d0b8xaa1ix"))))
      (properties `((upstream-name . "languageserver")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-callr
             r-collections
             r-fs
             r-jsonlite
             r-lintr
             r-r6
             r-roxygen2
             r-stringi
             r-styler
             r-xml2
             r-xmlparsedata))
      (native-inputs (list r-mockery r-testthat))
      (home-page "https://github.com/REditorSupport/languageserver")
      (synopsis "Language Server for R")
      (description
       "This package provides an implementation of the Language Server
Protocol for R.  The
@url{https://microsoft.github.io/language-server-protocol/,Language Server
protocol} is used by an editor client to integrate features like auto
completion.")
      (license license:expat))))

(define-public python-rpy2
  (package
    (name "python-rpy2")
    (version "3.5.17")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rpy2" version))
        (sha256
         (base32
          "10nmydlbmi0vyim7sx71isx3z2mnnfjmhf3248cicy9x1z1hizyv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: ...trying to load module _rinterface_cffi_api: ERROR:
          ;;
          ;; See: <https://github.com/rpy2/rpy2/issues/1034>.
          (delete 'sanity-check))))
    (propagated-inputs
     (list python-cffi
           python-jinja2
           python-numpy
           python-pandas
           python-pytz
           python-ipython
           python-tzlocal))
    (inputs
     (list icu4c
           libdeflate
           pcre
           python-numpy
           readline
           r-minimal
           r-survival
           r-ggplot2
           r-rsqlite
           r-dplyr
           r-dbplyr
           zlib))
    (native-inputs
     (list python-pytest-8
           python-setuptools))
    (home-page "https://rpy2.github.io")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    ;; Any of these licenses can be picked for the R interface.  The whole
    ;; project is released under GPLv2+ according to the license declaration
    ;; in "setup.py".
    (license (list license:mpl2.0 license:gpl2+ license:lgpl2.1+))))

(define-public python-rchitect
  (package
    (name "python-rchitect")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/randy3k/rchitect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r41ad8mk7bmqsw96sizahzvz1z6cp4rpll166y0yhwdrv86nmj7"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cffi python-packaging python-six))
    (native-inputs (list python-pytest
                         python-pytest-cov
                         python-pytest-mock
                         python-setuptools
                         python-wheel
                         ;; R dependencies needed only for testing.
                         r-minimal
                         r-reticulate))
    (home-page "https://github.com/randy3k/rchitect")
    (synopsis "Mapping R API to Python")
    (description
     "rchitect provides access to R functionality from Python.  Its
main use is as the driver for radian, the R console.")
    (license license:expat)))

(define-public python-radian
  (package
    (name "python-radian")
    (version "0.6.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/randy3k/radian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "089ys59cnw5l5k0656arhn805j8pkw21q4qf7gq9p9hifi1lpnpm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'set-home
                     (lambda _
                       ;; During tests radian wants to write history files to
                       ;; $HOME which causes tests to fail when that does not
                       ;; exist. Test fails then look like
                       ;; "Exception: value is " with the value being empty.
                       (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-prompt-toolkit python-pygments
                             python-rchitect))
    (native-inputs (list python-pexpect
                         python-ptyprocess
                         python-pyte
                         python-pytest
                         python-setuptools
                         ;; Needed afaict only for
                         ;; `tests/test_reticulate.py::test_completion`.
                         python-jedi
                         ;; R dependencies needed only for testing.
                         r-askpass
                         r-minimal
                         r-reticulate
                         ;; Needed for sh tests.
                         git-minimal))
    (home-page "https://github.com/randy3k/radian")
    (synopsis "R console")
    (description
     "Radian is an alternative console for the R program with multiline
editing and rich syntax highlight.  One would consider Radian as a IPython
clone for R, though its design is more aligned to Julia.")
    (license license:expat)))

(define-public emacs-ess
  (package
    (name "emacs-ess")
    (version "25.01.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-ess/ESS")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0v56b47qidpyxvyk0q487qxhj9si0jkm852frl832iraks02l5h5"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Stop ESS from trying to bundle an external julia-mode.el.
            (substitute* "lisp/Makefile"
              ((" \\$\\(JULIAS)") "")
              (("\ttest.*julia-mode.*\\.el") ""))
            ;; Only build docs in info format.
            (substitute* "doc/Makefile"
              (("all  : info text")
               "all  : info")
              (("install: install-info install-other-docs")
               "install: install-info"))
            ;; Stop install-info from trying to update the info directory.
            (substitute* "doc/Makefile"
              ((".*/dir.*") ""))
            ;; Avoid generating ess-autoloads.el twice.
            (substitute* "Makefile"
              (("all: lisp doc etc autoloads")
               "all: lisp doc etc"))
            ;; Install to correct directories.
            (substitute* "Makefile"
              (("mkdir -p \\$\\(ESSDESTDIR)")
               "$(MAKE) -C lisp install; $(MAKE) -C doc install")
              (("\\$\\(INSTALL) -R \\./\\* \\$\\(ESSDESTDIR)/")
               "$(MAKE) -C etc install"))))))
    (build-system gnu-build-system)
    (arguments
     (let ((base-directory (string-append "/share/emacs/site-lisp/"
                                          "ess-" version)))
       (list
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
        #:imported-modules `(,@%default-gnu-imported-modules
                             (guix build emacs-build-system)
                             (guix build emacs-utils))
        #:make-flags
        #~(list (string-append "PREFIX=" #$output)
                (string-append "ETCDIR=" #$output #$base-directory "/etc")
                (string-append "LISPDIR=" #$output #$base-directory)
                (string-append "INFODIR=" #$output "/share/info"))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'check 'skip-failing-tests
              (lambda _
                (let-syntax
                    ((disable-tests
                      (syntax-rules ()
                        ((_ ())
                         (syntax-error "test names list must not be empty"))
                        ((_ (test-name ...))
                         (substitute* (find-files "test" "\\.el$")
                           (((string-append "^\\(ert-deftest " test-name ".*")
                             all)
                            (string-append all "(skip-unless nil)\n"))
                           ...))))
                     (disable-etests  ;different test syntax
                      (syntax-rules ()
                        ((_ ())
                         (syntax-error "test names list must not be empty"))
                        ((_ (test-name ...))
                         (for-each
                          (lambda (file)
                            (emacs-batch-edit-file file
                              '(progn
                                (dolist (test (list test-name ...))
                                        (goto-char (point-min))
                                        (let ((s (format "etest-deftest %s "
                                                         test)))
                                          (when (search-forward s nil t)
                                            (beginning-of-line)
                                            (kill-sexp))))
                                (basic-save-buffer))))
                          (find-files "test" "\\.el$"))))))
                  (disable-tests ("ess--derive-connection-path"
                                  "ess-eval-line-test"
                                  "ess-eval-region-test"
                                  "ess-mock-remote-process"
                                  "ess-r-load-ESSR-github-fetch-no"
                                  "ess-r-load-ESSR-github-fetch-yes"
                                  "ess-set-working-directory-test"
                                  "ess-test-r-startup-directory"))
                  (disable-etests ("ess-r-eval-ns-env-roxy-tracebug-test"
                                   "ess-r-eval-sink-freeze-test"
                                   ;; Looks like an off-by-one error.
                                   "ess--command-browser-unscoped-essr")))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests? (invoke "make" "-C" "test" "ess"))))))))
    (native-inputs (list perl r-roxygen2 texinfo))
    (inputs (list emacs-minimal r-minimal))
    (propagated-inputs (list emacs-julia-mode))
    (home-page "https://ess.r-project.org/")
    (synopsis "Emacs mode for statistical analysis programs")
    (description
     "Emacs Speaks Statistics (ESS) is an add-on package for GNU Emacs.  It
is designed to support editing of scripts and interaction with various
statistical analysis programs such as R, Julia, and JAGS.")
    (license license:gpl3+)))

(define-public emacs-poly-r
  (package
    (name "emacs-poly-r")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polymode/poly-R")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4wx73jkngw5nbq1fa4jfhba6bsmyn6vnsf887x3xhb5v3ykhsg"))))
    (build-system emacs-build-system)
    (arguments
     (list #:test-command
           #~(list "emacs" "-Q" "-batch" "--load" "targets/melpa-init.el"
                   "--load" "targets/test.el")))
    (propagated-inputs
     (list emacs-ess emacs-poly-noweb emacs-polymode-markdown
           emacs-polymode))
    (properties '((upstream-name . "poly-R")))
    (home-page "https://github.com/polymode/poly-markdown")
    (synopsis "Polymodes for the R language")
    (description
     "This package provides a number of polymodes for working with mixed R
files, including Rmarkdown files.")
    (license license:gpl3+)))

(define-public python-pyreadstat
  (package
    (name "python-pyreadstat")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Roche/pyreadstat")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01b7zwvfl46sra0kvdvs19ggx150p4x7pkwl73ar2jda315kjfr4"))
       (patches (search-patches "python-pyreadstat-link-libiconv.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 92 passed
      #:test-backend #~'custom
      ;; The source also contains tests/test_version.py which checks the
      ;; version in __init__.py against the one in setup.py. Since this
      ;; requires texlive dependencies to run and is also not mentioned in
      ;; how_to_test.md, this test is skipped.
      #:test-flags #~(list "tests/test_basic.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cython
           python-setuptools))
    (inputs
     (list libiconv
           readstat
           zlib))
    (propagated-inputs
     (list python-narwhals-minimal
           python-pandas
           python-numpy))
    (home-page "https://github.com/Roche/pyreadstat")
    (synopsis "Read and write SAS, SPSS and Stata files into/from Pandas DataFrames")
    (description
     "This Python package can be used to read and write SAS, SPSS and Stata
files into/from Pandas DataFrames.  It is a wrapper around the C library
@code{readstat}.")
    (license license:asl2.0)))

(define-public r-mixedpower
  ;; This commit contains fixes for R>=4.2 and contains new features. A
  ;; newer release does not exist.
  (let ((commit "b2b87068546327d8f592d141e5482e0478c1b2ee")
        (revision "2"))
    (package
      (name "r-mixedpower")
      (version (git-version "2.0" revision commit))
      (source
        (origin
          ;; Not available on CRAN.
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/DejanDraschkow/mixedpower")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32 "0pys66a6c1k2fp5qk9grhzg1q1d3q51rggcilmdssimkmcsm234z"))))
      (properties `((upstream-name . "mixedpower")))
      (build-system r-build-system)
      (propagated-inputs
        (list r-doparallel r-foreach r-ggplot2 r-lme4 r-reshape2))
      (home-page "https://github.com/DejanDraschkow/mixedpower")
      (synopsis
        "Pilotdata based simulations for estimating power in linear mixed models")
      (description
       "Mixedpower uses pilotdata and a linear mixed model fitted with lme4 to
simulate new data sets.  Power is computed separate for every effect in the
model output as the relation of significant simulations to all simulations.
More conservative simulations as a protection against a bias in the pilotdata
are available as well as methods for plotting the results.")
      (license license:gpl3))))

(define-public r-colorway
  (let ((commit "8ba8f0026aba37752c6770de45bf53b1b0f48afc")
        (revision "1"))
    (package
      (name "r-colorway")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hypercompetent/colorway")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r6yhnzx3ll1z12pp6y8fqbrx7v18rcff2j2179bqy9ca2d2d94l"))))
      (properties `((upstream-name . "colorway")))
      (build-system r-build-system)
      (propagated-inputs (list r-dplyr r-ggplot2 r-rlang))
      (home-page "https://github.com/hypercompetent/colorway")
      (synopsis "Functions for colors in R")
      (description
       "This package provides a collection of (mostly simple) functions for
generating and manipulating colors in R.")
      (license license:gpl3))))

(define-public readstat
  (let ((commit "718d49155e327471ed9bf4a8c157f849f285b46c")
        (revision "0"))
  (package
    (name "readstat")
    (version (git-version "1.1.9" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WizardMac/ReadStat")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "121axcx4shd0fqlcr1mk7y841sd59r9mx473c4av8gs81xfhcg0h"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal libtool))
    (inputs
     (list zlib))
    (home-page "https://github.com/WizardMac/ReadStat")
    (synopsis "Convert SAS, Stata, and SPSS files")
    (description "Command-line tool and C library for reading files from
popular stats packages like SAS, Stata and SPSS.")
    (license license:expat))))

(define-public ruby-enumerable-statistics
  (package
    (name "ruby-enumerable-statistics")
    (version "2.0.7")
    ;; Source at RubyGems.org doesn't have tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mrkn/enumerable-statistics.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a8k2dvm1v0g6hcrbnzy0f7d63hdmpmldfdkl8wr32nbl05xnifa"))
              (modules '((guix build utils)))
              (snippet `(begin
                          (substitute* "enumerable-statistics.gemspec"
                            ;; benchmark-driver gem is used for
                            ;; performance benchmarking, and isn't
                            ;; needed for tests.
                            (("spec.add_development_dependency \"benchmark-driver\"\n")
                             ""))))))
    (build-system ruby-build-system)
    (native-inputs (list bundler
                         ruby-rake
                         ruby-rake-compiler
                         ruby-rspec
                         ruby-test-unit
                         ruby-fuubar
                         ruby-yard))
    (synopsis "Library which provides statistics features for Enumerable")
    (description
     "@code{Enumerable::Statistics} provides some methods to calculate
statistical summary in arrays and enumerables.")
    (home-page "https://github.com/mrkn/enumerable-statistics")
    (license license:expat)))

(define-public python-pgmpy
  (package
    (name "python-pgmpy")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)  ;pypi package does not include test data
       (uri (git-reference
             (url "https://github.com/pgmpy/pgmpy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hg6wrg3jcac71zn4gknni1wrn38wa86ka3sgp2bndz59mx6sr2s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              "-k" (string-join
                    ;; AssertionError: False is not true
                    (list "not test_query_evidence"
                          ;; ValueError: Experimental support for categorical
                          ;; data is not implemented for current tree method
                          ;; yet.
                          "test_pillai"
                          "test_estimate_with_cache_no_llm_calls"
                          "test_estimate_with_orientations"
                          ;; _flapack.error: (liwork>=max(1,10*n)||liwork==-1)
                          ;; failed for 10th keyword liwork: dsyevr:liwork=1
                          "test_estimate"
                          "test_score_bnlearn"
                          "test_score_manual"
                          ;; FileNotFoundError in tearDown: removes missing
                          ;; 'dog_problem.xml'.
                          "test_get_edges"
                          "test_get_parents"
                          ;; flaky test
                          "test_gcm")
                    " and not "))))
    (propagated-inputs (list python-daft
                             python-joblib
                             python-networkx
                             python-numpy
                             python-opt-einsum
                             python-pandas
                             python-pyparsing
                             python-pytorch
                             python-scikit-learn
                             python-scipy
                             python-statsmodels
                             python-tqdm))
    (native-inputs (list python-mock
                         python-pyro-ppl
                         python-pytest
                         python-pytest-xdist
                         python-setuptools
                         python-xgboost))
    (home-page "https://github.com/pgmpy/pgmpy")
    (synopsis "Probabilistic Graphical Models library")
    (description
     "This package provides a library for Probabilistic Graphical Models.  It
can be used for learning (Structure and Parameter), inference (Probabilistic
and Causal), and simulations in Bayesian Networks.")
    (license license:expat)))

(define-public xlispstat
  (let ((commit "f1bea6053df658ee48612bf1f63c35de99e2c649")
        (revision "0"))
    (package
      (name "xlispstat")
      (version (git-version "3.52.23" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jhbadger/xlispstat.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1p0cmgy19kbkxia139cb5w9dnkp2cdqp5n3baag6cq3prn3n71mf"))
                (patches
                 (search-patches
                  "xlispstat-fix-compilation-with-modern-gcc.patch"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f   ; Parallel builds are not supported
         #:configure-flags (list "--with-gcc")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-output-to-file "exit.lsp"
                   (lambda () (display "(exit)")))
                 (invoke "./xlisp" "tests/test" "exit")))))))
      (inputs (list tcsh
                    libx11
                    libxmu
                    libxext
                    libxpm
                    libxaw
                    ncurses
                    gnuplot))
      (native-inputs (list pkg-config))
      (synopsis "Statistical analysis environment with interactive graphics")
      (description "XLISP-STAT is a statistical environment based on a Lisp
dialect called XLISP.  To facilitate statistical computations, standard
functions for addition, logarithms, etc., have been modified to operate on
lists and arrays of numbers, and a number of basic statistical functions have
been added.  Many of these functions have been written in Lisp, and additional
functions can be added easily by a user.  Several basic forms of plots,
including histograms, scatterplots, rotatable plots and scatterplot matrices
are provided.  These plots support various forms of interactive highlighting
operations and can be linked so points highlighted in one plot will be
highlighted in all linked plots.  Interactions with the plots are controlled
by the mouse, menus and dialog boxes.  An object-oriented programming system
is used to allow menus, dialogs, and the response to mouse actions to be
 customized.")
      (home-page "https://homepage.divms.uiowa.edu/~luke/xls/xlsinfo/")
      (license license:expat))))
